{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the POS tags and Chunk tags derived from
-- the Conll2000 training corpus.

module NLP.TagSets.ConllChunks (
    module NLP.TagSets.ConllChunks
    , ChunkTag (..)
    , ChunkTags(..)
    ) where

import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import  NLP.Tags  (POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
--import NLP.Types.General
import Data.Utilities

instance ChunkTags ChunkTag

-- | Phrase chunk tags defined for the Conll task.
data ChunkTag = ADJP
           | ADVP
           | CONJP
           | INTJ
           | LST
           | NP -- ^ Noun Phrase.
           | PP -- ^ Prepositional Phrase.
           | PRT
           | SBAR
           | UCP
           | VP -- ^ Verb Phrase.
           | O -- ^ "out"; not a chunk.
           | UNK
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance Arbitrary ChunkTag where
  arbitrary = elements [minBound..]

instance Serialize ChunkTag


instance TagsetIDs ChunkTag where
    tagsetURL _  = "Conll task chunk tags"

