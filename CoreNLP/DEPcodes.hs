{-----------------------------------------------------------------------------
--
-- Module      :  Dependency codes for UD
--
-- |
--
-----------------------------------------------------------------------------}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
         #-}
{-# LANGUAGE DeriveGeneric #-}  -- for RawDEPtag

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

import Data.Serialize (Serialize) -- for RawDEPtag
import Data.Serialize.Text ()
import GHC.Generics
import Uniform.Zero

import qualified Data.Map as Map
import  Data.Map (Map (..))
import Data.Utilities
import qualified Data.Text as T
import Data.Text (Text)


--class (Ord a, Eq a, Read a, Show a) => POStags a where
---- , Generic a, Serialize a
--    fromTag :: a -> Text
--    parseTag :: Text -> a
--    tagUNK :: a

class (Ord a, Eq a, Read a, Show a) => DEPtags a where
  fromDEPtag :: a -> Text
  parseDEPtag :: Text -> a
--  propertyDEP :: a -> Text
--  -- produce a property text for RDF
-- use the fromDEPtag and convert to lowercase

  tagDEPunk :: a
  notDEPtag :: a
--  default notChunkTag :: Bounded a => a
--  notChunkTag = maxBound

instance DEPtags DepCode where
  fromDEPtag (DepCode c1 c2) = if c2==Dep2Zero then showT c1
                                               else T.concat [showT c1, ":", showT c2]
  fromDEPtag x = error ("fromDEPtag in DEPcodes " ++ show x)

  parseDEPtag   = readDepCode
  tagDEPunk  =  DepCode DepUnk Dep2Zero

--  propertyDEP =

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
instance Zeros DepCode where zero = DepUnknown "constant zero"

--
map1 :: Map DepCode1 Text
map1 = Map.fromList $ zip [ACL .. ] (map showT [ACL ..])
--
map2 :: Map DepCode2 Text
map2 = Map.fromList $ zip [RELCL .. ] (map showT [RELCL ..])

readDepCode :: Text -> DepCode
readDepCode t = maybe unk conv (splitIn2By ":" (T.toUpper t)) -- at least french model produces root

--    case splitIn2By ":" t of
--    Nothing -> unk
--    Just (a,Nothing) -> maybe unk (\c -> DepCode c Dep2Zero) c1
--    Just (a, Just b) ->


--readDepCode t = case length ts of
--                    0 -> unk
--                    1 -> maybe unk (\c  -> DepCode c  Dep2Zero) c1
--                    2 -> case c2 of
--                            Nothing -> unk
--                            Just c22 -> case c1 of
--                                Nothing -> unk
--                                Just c11 -> DepCode c11 c22
--
--
    where
            conv (a, mb) =  case conv2 (a, mb) of
                    (Nothing, _) -> unk
                    (Just a1, Nothing) -> DepCode a1 Dep2Zero
                    (Just a1, Just b1) -> DepCode a1 b1


--            ts = T.splitOn ":" t :: [Text]
            unk = DepUnknown t
            c1 a =    (reverseLookup map1 a) :: Maybe DepCode1
            c2 b =    (reverseLookup map2 b):: Maybe DepCode2

            conv2 (a, mb) = (c1 a, flatMaybe $ fmap c2 mb) :: (Maybe DepCode1, Maybe DepCode2)



--reverseMap:: Map a Text -> Map  Text a
--reverseMap m1 = Map.fromList [ (b,a) | (a,b) <- Map.assocs m1]


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


------------------------------------------------------------------- D E P tags

-- | A fallback Dependency tag instance.

newtype RawDEPtag = RawDEPtag Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawDEPtag

instance DEPtags RawDEPtag where
  fromDEPtag (RawDEPtag ch) = ch
  parseDEPtag txt =  RawDEPtag txt
