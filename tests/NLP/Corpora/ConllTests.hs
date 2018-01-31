{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NLP.Corpora.ConllTests where

import Test.Framework


import  NLP.Types.Tags

import  NLP.Corpora.Conll
import  NLP.Corpora.ConllNER
import NLP.Types.Tags


prop_tagsRoundTrip ::  POStag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

prop_nerTagsRoundTrip :: NERtag -> Bool
prop_nerTagsRoundTrip tag = tag == (parseNERTag . fromNERTag) tag
--
--prop_chunkTagsRoundTrip :: C.Chunk -> Bool
--prop_chunkTagsRoundTrip tag = tag == (fromRight . parseChunk . fromChunk) tag
--
--fromRight :: Either a b -> b
--fromRight (Left  _) = error "Expected a 'Right' value"
--fromRight (Right v) = v

