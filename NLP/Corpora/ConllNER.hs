{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Data types representing the NER tags
-- the Conll2000 training corpus.

module NLP.Corpora.ConllNER (
    module NLP.Corpora.ConllNER
        , NERtag (..)
        , NERtags (..)
--    , POStags (..)
--    , POStag(..)
    ) where

import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import  NLP.Types.Tags  (NERtags (..), POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
--import NLP.Types.General
import Data.Utilities

-- | Named entity categories defined for the Conll 2003 task.
data NERtag = PER
            | ORG
            | LOC
            | MISC
            | UNK
  deriving (Read, Show, Ord, Eq, Enum, Bounded, Generic, Serialize)

instance Arbitrary NERtag where
  arbitrary = elements [minBound..]

--instance Serialize NERtag
instance NERtags NERtag

instance TagsetIDs NERtag where
    tagsetURL _  = "Conll"


