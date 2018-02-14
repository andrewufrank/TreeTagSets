{-----------------------------------------------------------------------------
--
-- Module      :  Dependency and other Codes
--
-- |
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
         #-}

module CoreNLP.DEPcodes (module CoreNLP.DEPcodes



--        DepCode1(..), DepCode2 (..), DepCode
--        , isROOT, isPUNCT
----        , hasDepCode
--        , makeSimpleDepCode, makeDepCode
--        , Pos (..)  -- , Unk
--        , NERTag (..)
--        , isVerbCode, isNounCode, isPunctuation, isAdjective
--        , isClosedClass
--        , isSimpleCode
--        , isPOSpunctuation
--        , coarsePOS
--        , readDepCodes, showDepCodes
--        , SpeakerTag (..), readSpeakerTag
--        , Conll.Tag (..)

        )
         where

import           Test.Framework

import qualified Data.Map as Map
import  Data.Map (Map (..))
import Data.Utilities
import qualified Data.Text as T
import Data.Text (Text)


class (Ord a, Eq a, Read a, Show a) => POStags a where
-- , Generic a, Serialize a
    fromTag :: a -> Text
    parseTag :: Text -> a
    tagUNK :: a

class (Ord a, Eq a, Read a, Show a) => DEPtags a where
  fromDEPtag :: a -> Text
  parseDEPtag :: Text -> a
  tagDEPunk :: a
  notDEPtag :: a
--  default notChunkTag :: Bounded a => a
--  notChunkTag = maxBound

instance DEPtags DepCode where
  fromDEPtag (DepCode c1 c2) = if c2==Dep2Zero then showT c1
                                               else T.concat [showT c1, ":", showT c2]
  parseDEPtag   = readDepCode
  tagDEPunk  =  DepCode DepUnk Dep2Zero

data DepCode = DepCode {d1::DepCode1
                        , d2 :: DepCode2
                        }
            | DepUnknown {s :: Text }
                deriving (Show, Read, Eq, Ord)

--instance CharChains2 DepCode Text where
instance Arbitrary DepCode1 where
  arbitrary = elements [minBound ..]
instance Arbitrary DepCode2 where
  arbitrary = elements [minBound ..]

--
map1 :: Map DepCode1 Text
map1 = Map.fromList $ zip [ACL .. ] (map showT [ACL ..])
--
map2 :: Map DepCode2 Text
map2 = Map.fromList $ zip [RELCL .. ] (map showT [RELCL ..])

readDepCode :: Text -> DepCode
readDepCode t = case length ts of
                    0 -> unk
                    1 -> maybe unk (\c  -> DepCode c  Dep2Zero) c1
                    2 -> case c2 of
                            Nothing -> unk
                            Just c22 -> case c1 of
                                Nothing -> unk
                                Just c11 -> DepCode c11 c22


    where
            ts = T.splitOn ":" t :: [Text]
            unk = DepUnknown t
            c1 =    (reverseLookup map1 . head $ ts) :: Maybe DepCode1
            c2 =    (reverseLookup map2 . head . tail $ ts):: Maybe DepCode2

splitIn2By :: Text -> Text -> Maybe (Text, Maybe Text)
-- split a text in two pieces, separated - if two are present
splitIn2By sep t = case T.splitOn sep t of
    [] -> Nothing
    [a] -> Just (a, Nothing)
    [a,b] -> Just (a, Just b)
    _ -> Nothing

test_splitok = assertEqual (Just ("a", Just "b")) (splitIn2By ":" "a:b")
test_splitnok1 = assertEqual (Nothing) (splitIn2By ":" "a:b:c")
test_splitok2 = assertEqual (Just ("a ", Nothing)) (splitIn2By ":" "a ")
test_splitnok3 = assertEqual (Just("",Just "b")) (splitIn2By ":" ":b")
test_splitnok4 = assertEqual (Just(" ",Nothing)) (splitIn2By ":" " ")

test_1a = assertEqual (DepCode ACL Dep2Zero) (readDepCode "ACL")
test_2a = assertEqual (DepCode AUX ON) (readDepCode "AUX:ON")
test_2b = assertEqual (DepCode AUX Dep2Zero) (readDepCode "AUX")

test_3a = assertEqual "AUX:ON" (fromDEPtag $ DepCode AUX ON)
test_3b = assertEqual "AUX" (fromDEPtag $ DepCode AUX Dep2Zero)

test_depCode_cc = assertEqual (DepCode CC Dep2Zero) (readDepCode "CC")

--reverseMap:: Map a Text -> Map  Text a
--reverseMap m1 = Map.fromList [ (b,a) | (a,b) <- Map.assocs m1]

reverseLookup :: Ord a => Map a Text -> Text -> Maybe a
reverseLookup m1 a1  = Map.lookup a1 (reverseMap m1)

data DepCode1 = ACL
                | ADVCL
                | ADVMOD
                | AMOD
                | APPOS
                | AUX
                | AUXPASS
                | CASE
                | CC  -- was CC but gives conflict with Conll.Tag
                | CCOMP
                | COMPOUND
                | CONJ
                | COP
                | CSUBJ
                | CSUBJPASS
                | DEP
                | DET
                | DISCOURSE
                | DISLOCATED
                | DOBJ
                | EXPL
                | FOREIGN
                | GOESWITH
                | IOBJ
                | LIST
                | MARK
                | MWE
                | NAME
                | NEG
                | NMOD
                | NSUBJ
                | NSUBJPASS
                | NUMMOD
                | PARATAXIS
                | PUNCT
                | REF   -- ??
                | REMNANT
                | REPARANDUM
                | ROOT
                | VOCATIVE
                | XCOMP
                | DepUnk

                deriving (Show, Read, Eq, Ord, Enum, Bounded)
--instance CharChains2 DepCode1 Text where
--    show' = s2t . show

data DepCode2 = RELCL
            | AS  -- is this all prepositions?
            | ON
            | INTO
            | ABOUT
            | UPON
            | BUT
            | OF
            | IN  -- was IN, but conflicts with Conll.Tag
            | OVER
            | DOWN
            | AFTER
            | BEHIND
            | BY
            | WITH
            | LIKE
            | THAN
            | AGAINST
            | OR
            | DURING
            | FOR
            | TO  -- was TO, but conflicts with Conll.Tag
            | AT
            | AND
            | AGENT
            | POSS
            | TMOD
            | NPMOD
            | PRT
            | PREDET
            | TOWARDS
--            | MissingDepCode2 Text
            | Dep2Zero

    deriving (Show, Read, Eq, Ord, Enum, Bounded )



isROOT = (ROOT==) . d1
isPUNCT = (PUNCT==) . d1

hasDepCode:: DepCode1 -> DepCode2 -> DepCode -> Bool
-- | test if d has major or minor depcode
hasDepCode maj min d = d1 d == maj && d2 d == min

isSimpleCode ::  DepCode1 -> DepCode -> Bool
isSimpleCode aa d = d1 d == aa

makeDepCode :: DepCode1 -> DepCode2 -> DepCode
makeDepCode maj min = DepCode maj min

makeSimpleDepCode :: DepCode1 -> DepCode
makeSimpleDepCode maj = DepCode maj Dep2Zero

showDepCodes :: DepCode -> Text
showDepCodes (DepCode dd1 dd2)  = if dd2==Dep2Zero then showT dd1
        else (T.concat [showT dd1, ":", showT dd2])


----instance Zeros DepCode where zero = (z)
--instance Zeros DepCode1 where zero =  DepUnk
--instance Zeros DepCode2 where zero =  DepZero
