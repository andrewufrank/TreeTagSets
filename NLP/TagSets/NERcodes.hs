{-----------------------------------------------------------------------------
--
-- Module      :  NER codes as used in coreNLP
--
-- |
--
-----------------------------------------------------------------------------}
{-# LANGUAGE        MultiParamTypeClasses
    , OverloadedStrings
        , DeriveAnyClass
        , DeriveGeneric
        #-}

module NLP.TagSets.NERcodes (module NLP.TagSets.NERcodes
 , module NLP.Tags
        )
         where
import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Utilities
import Text.Read (readEither)
import GHC.Generics
import NLP.Tags

instance NERtags NERtag where
  toNERtag txt = either (const NERunk) id (readEitherT txt2)
    where
        txt2 = T.replace "-" "_" txt
  unkNERtag = NERunk
  fromNERtag = showT

--fromNERtagNormalized I_LOC  = fromNERtag LOC
----fromNERtagNormalized LUG = fromNERtag LOC
--
--fromNERtagNormalized I_ORG = fromNERtag ORG
--fromNERtagNormalized I_PER = fromNERtag PER
----fromNERtagNormalized PERS = fromNERtag PER
--fromNERtagNormalized I_MISC = fromNERtag MISC
--fromNERtagNormalized x = error ("fromNERtagNormalized missing for " ++ show x)
--
--parseNERtagList :: [Text] -> [NERtag]
--parseNERtagList [] = []
--parseNERtagList [a] = [toNERtag a]
--parseNERtagList (a:as) = toNERtag a : map NERtagValue as
--
--isAnUnknownNER  (NERunk a) = True
--isAnUnknownNER  _ = False

-- | Named entity categories defined for the Conll 2003 task.
data NERtag = PER
            | ORG
            | LOC
            | MISC
            | NERunk
            -- found in Stanford CoreNLP 3.5.2
            -- Time, Location, Organization, Person, Money, Percent, Date
            | O  -- does this stand for other = nothing?
            | NUMBER
            | PERSON
            | DURATION
            | DATE
            | SET
            | TIME
            | ORDINAL
            | LOCATION
            | ORGANIZATION
            | MONEY
            | PERCENT

  deriving (Show, Read, Eq, Ord, Generic)
         -- ,  Enum, Bounded)



