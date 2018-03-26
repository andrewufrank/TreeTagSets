{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE  OverloadedStrings
----        , TypeSynonymInstances
----        , FlexibleInstances
----        , DeriveAnyClass
         #-}
module NLP.TagSets.ItalianTinTTests where

import Test.Framework

import  NLP.TagSets.ItalianTinT
import NLP.Tags


prop_tagsRoundTrip ::  POStag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

test_erd = assertEqual EplusRD (parseTag "E+RD")

--prop_nerTagsRoundTrip :: C.NERtag -> Bool
--prop_nerTagsRoundTrip tag = tag == (fromRight . parseNERTag . fromNERTag) tag
--
--prop_chunkTagsRoundTrip :: C.Chunk -> Bool
--prop_chunkTagsRoundTrip tag = tag == (fromRight . parseChunk . fromChunk) tag
--
--fromRight :: Either a b -> b
--fromRight (Left  _) = error "Expected a 'Right' value"
--fromRight (Right v) = v

