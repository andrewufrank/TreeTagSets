{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Data types representing the NER tags
-- the Conll2000 training corpus.

module xNLP.Corpora.ConllNER (
    module NLP.Corpora.ConllNER
 , module NLP.Tags
--    , POStags (..)
--    , POStag(..)
    ) where

import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import  NLP.Types.Tags  (POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
--import NLP.Types.General
import Data.Utilities

import CoreNLP.NERcodes (NERtags (..))
--
---- | Named entity categories defined for the Conll 2003 task.
--data NERtag = PER
--            | ORG
--            | LOC
--            | MISC
--            | UNK
--  deriving (Read, Show, Ord, Eq, Enum, Bounded, Generic, Serialize)
--
--instance Arbitrary NERtag where
--  arbitrary = elements [minBound..]
--
----instance Serialize NERtag
--instance NERtags NERtag
--    fromNERtagNormalized = error "fromNERtagNormalized not implemented"
--    parseNer
--instance TagsetIDs NERtag where
--    tagsetURL _  = "Conll"
--

