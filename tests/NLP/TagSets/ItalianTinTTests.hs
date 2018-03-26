{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE  OverloadedStrings
----        , TypeSynonymInstances
----        , FlexibleInstances
----        , DeriveAnyClass
         #-}
module NLP.TagSets.ItalianTinTTests where

import Test.Framework

import  NLP.TagSets.ItalianTinT
--import NLP.Tags

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]


prop_tagsRoundTrip ::  POStag -> Bool
prop_tagsRoundTrip tag = tag == (toPOStag . fromPOStag) tag

test_erd = assertEqual EplusRD (toPOStag "E+RD")

--prop_nerTagsRoundTrip :: C.NERtag -> Bool
--prop_nerTagsRoundTrip tag = tag == (fromRight . parseNERTag . fromNERTag) tag
--
--prop_chunkTagsRoundTrip :: C.Chunk -> Bool
--prop_chunkTagsRoundTrip tag = tag == (fromRight . parseChunk . fromChunk) tag
--
--fromRight :: Either a b -> b
--fromRight (Left  _) = error "Expected a 'Right' value"
--fromRight (Right v) = v

