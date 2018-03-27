{-----------------------------------------------------------------------------
--
-- Module      :  Dependency codes for UD
--
-- | does not separate UD v1 or v2
-- two modules could be made if necessary
--
-----------------------------------------------------------------------------}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
         #-}
{-# LANGUAGE DeriveGeneric #-}  -- for RawDEPtag

module NLP.TagSets.DEPcodes (module NLP.TagSets.DEPcodes
 , module NLP.Tags
        )
         where

import NLP.Tags

import GHC.Generics

import qualified Data.Map as Map
import  Data.Map (Map (..))
import Data.Utilities
import qualified Data.Text as T
import Data.Text (Text)


instance DEPtags DepCode where
  fromDEPtag (DepCode c1 c2) = if c2==Dep2zero
        then showT c1
        else T.concat [showT c1, ":", showT c2]
  fromDEPtag (DepUnknown s) = s

  toDEPtag t  = readDepCode t
  unkDEPtag  =  DepCode DepUnk Dep2zero
  -- ^ contains the value, cannot be used for testing after parsing!

data DepCode = DepCode {d1::DepCode1
                        , d2 :: DepCode2
                        }
            | DepUnknown {s :: Text }
                deriving (Show, Read, Eq, Ord, Generic)


--
map1 :: Map DepCode1 Text
map1 = Map.fromList $ zip [ACL .. ] (map showT [ACL ..])
--
map2 :: Map DepCode2 Text
map2 = Map.fromList $ zip [RELCL .. ] (map showT [RELCL ..])

readDepCode :: Text -> DepCode
readDepCode t = maybe unk conv (splitIn2By ":" (T.toUpper t))
    where
            conv (a, mb) =  case conv2 (a, mb) of
                    (Nothing, _) -> unk
                    (Just a1, Nothing) -> DepCode a1 Dep2zero
                    (Just a1, Just b1) -> DepCode a1 b1
            unk = DepUnknown t
            c1 a =    (reverseLookup map1 a) :: Maybe DepCode1
            c2 b =    (reverseLookup map2 b):: Maybe DepCode2

            conv2 (a, mb) = (c1 a, joinMaybe $ fmap c2 mb)
                         :: (Maybe DepCode1, Maybe DepCode2)


data DepCode1 = ACL
                | ADVCL
                | ADVMOD
                | AMOD
                | APPOS
                | AUX
                | AUXPASS
                | CASE
                | CC
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
                | FLAT  -- used in hs-conllu
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
                | ROOT   -- used in french model
                | VOCATIVE
                | XCOMP
                | DepUnk

        deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data DepCode2 = RELCL
            | AS  -- is this all prepositions?
            | ON
            | INTO
            | ABOUT
            | UPON
            | BUT
            | OF
            | IN
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
            | TO
            | AT
            | AND
            | AGENT
            | POSS
            | TMOD
            | NPMOD
            | PRT
            | PREDET
            | TOWARDS
            | Dep2zero

    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

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
makeSimpleDepCode maj = DepCode maj Dep2zero

showDepCodes :: DepCode -> Text
showDepCodes (DepCode dd1 dd2)  = if dd2==Dep2zero then showT dd1
        else (T.concat [showT dd1, ":", showT dd2])



