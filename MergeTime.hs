{-# LANGUAGE FlexibleContexts, PartialTypeSignatures #-}
module Main( main ) where

import Control.Applicative
import Control.Monad (unless, join)
import Options

import Options.Applicative hiding (arguments)
import qualified Options.Applicative as Opt

import Control.Lens
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.XPathEval
import Text.HandsomeSoup

import Data.Char
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.Function

import Data.Algorithm.Diff
import Data.Algorithm.DiffContext
import Data.Algorithm.DiffOutput

getTier name = css "TIER" >>> hasAttrValue "TIER_ID" (== name)

getAnnotWord = css "ALIGNABLE_ANNOTATION" >>> (getAnnotValue &&& getAttrValue "TIME_SLOT_REF1" &&& getAttrValue "TIME_SLOT_REF2")

-- getTimeSlotRef1 :: ArrowXml cat => cat (_ XNode) String
-- getTimeSlotRef1 = multi (isElem >>> hasName "ALIGNABLE_ANNOTATION" >>> getChildren >>> getAttrValue "TIME_SLOT_REF1")

-- getTimeSlotRef2  :: ArrowXml cat => cat (_ XNode) String
-- getTimeSlotRef2 = css "ALIGNABLE_ANNOTATION" >>> getAttrValue "TIME_SLOT_REF2"

getAnnotValue = css "ANNOTATION_VALUE" >>> getChildren >>> getText 

timeCorresponce flex timed doc = do
    o <- runX $ doc >>> getTier flex >>> getAnnotWord
    t <- runX $ doc >>> getTier timed >>> getAnnotWord
    let diffs = getDiffBy ((==) `on` fst) o t
    return (o, t)
    return diffs

data Correspond a = Correspond [a] [a]
                  deriving (Show, Ord, Eq, Read)

getCorrespond :: [Diff a] -> [([a], [a])]
getCorrespond = f' ([], [])
    where
      f' ([], []) [] = []
      f' (a, b) [] = [(reverse a, reverse b)]
      f' (a, b) (First a' : xs) = f' (a' : a, b) xs
      f' (a, b) (Second b' : xs) = f' (a, b' : b) xs
      f' (a, b) (Both a' b' : xs) = f' (a, b) [] ++ ([a'], [b']) : f' ([], []) xs

instance Functor Diff where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second (f b)
    fmap f (Both a b) = Both (f a) (f b)

expandCorresponcence [] _ = []
expandCorresponcence _ [] = []
expandCorresponcence (a:as) [b]  = (a, b) : expandCorresponcence as [b]
expandCorresponcence [a] (b:bs)  = (a, b) : expandCorresponcence [a] bs
expandCorresponcence (a:as) (b:bs)  = (a, b) : expandCorresponcence as bs

{-
application	:: IOStateArrow s b XmlTree -> String -> IOSArrow b Int
application cfg src dst
    = configSysVars cfg                                           -- (0)
      >>>
      readDocument [] src
      >>>
      processChildren (processDocumentRootElement `when` isElem)  -- (1)
      >>>
      writeDocument [] dst                                        -- (3)
      >>>
      getErrStatus
-}

processDocumentRootElement :: IOSArrow XmlTree XmlTree
processDocumentRootElement
    = this         -- substitute this by the real application

substTimeRef1 substMap = getTier "A_word-txt-itl-fonipa" >>> css "ALIGNABLE_ANNOTATION" >>> getAttrValue "TIME_SLOT_REF1" >>> arr (substMap M.!) >>> mkText
substTimeRef2 substMap = getTier "A_word-txt-itl-fonipa" >>> css "ALIGNABLE_ANNOTATION" >>> getAttrValue "TIME_SLOT_REF2" >>> arr (substMap M.!) >>> mkText

substTimeSpecs      :: ArrowXml a => Map String String -> a XmlTree XmlTree
substTimeSpecs sts
    = processTopDown editHRef
    where
    editHRef
        = processAttrl ( changeAttrValue (applySubst sts)
                         `when`
                         (hasName "TIME_SLOT_REF1" `orElse` hasName "TIME_SLOT_REF2")
                       )
          -- `when`
          -- ( isElem >>> hasName "TIME_SLOT" ) 
        where

        applySubst :: Map String String -> String -> String
        applySubst sts ts
            = M.findWithDefault ts ts sts

mergeTime opts = do
      let doc = readDocument [withWarnings no] src
      diff <- timeCorresponce targetTier referenceTier doc
      let corr = getCorrespond . map (fmap snd) $ diff
      let subst = concatMap (uncurry expandCorresponcence) . over (mapped.both) (concatMap (\(a,b) -> [a,b])) $ corr
      -- let substMap = M.fromListWith S.union . over (mapped._2) S.singleton $ subst
      let substMap = M.fromList subst
      runX $ doc >>> substTimeSpecs substMap >>> writeDocument [] dst >>> getErrStatus
      return ()
      -- return substMap
  where
    message = unless (optQuiet opts) . putStrLn
    dst = optOutputFile opts
    src = optInputFile opts
    targetTier = optTargetTier opts
    referenceTier = optReferenceTier opts


-- main :: IO ()
-- main = runSubcommand
--     [ subcommand "hello" hello
--     , subcommand "bye" bye
--     , subcommand "merge-time" mergeTime
--     ]

data Sample = Sample
  { optInputFile :: String
  , optOutputFile :: String
  , optTargetTier :: String
  , optReferenceTier :: String
  , optQuiet :: Bool }

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "input"
        <> metavar "FILENAME"
        <> help "Input Elan filename" )
     <*> strOption
         ( long "output"
        <> metavar "FILENAME"
        <> help "Output Elan filename" )
     <*> strOption
         ( long "target-tier"
        <> metavar "TIERNAME"
        <> help "The name of a tier whose time information will be changed." )
     <*> strOption
         ( long "reference-tier"
        <> metavar "TIERNAME"
        <> help "The name of a tier whose time information is used to modify the target tier." )
     <*> switch
         ( long "quiet"
        <> help "Whether to be quiet" )

greet :: Sample -> IO ()
greet (Sample h _ _ _ False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= mergeTime
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Merge two tiers' time information."
     <> header "elan-merge-time -- A manipulator of Elan data")
