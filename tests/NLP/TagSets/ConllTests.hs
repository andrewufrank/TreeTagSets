{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
         #-}

module NLP.TagSets.ConllTests where

import Test.Framework


--import  NLP.Tags

import  NLP.TagSets.Conll
--import  NLP.TagSets.ConllNER
--import NLP.TagSets.ConllChunks

--import Data.Aeson
--import GHC.Exts

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]

prop_tagsRoundTrip ::  POStag -> Bool
prop_tagsRoundTrip tag = tag == (toPOStag . fromPOStag) tag

unkP =unkPOStag :: POStag
test_unk = assertEqual unkP (toPOStag "xy99")
test_unk2 = assertEqual "Unk" (fromPOStag unkP)

--prop_nerTagsRoundTrip :: NERtag -> Bool
--prop_nerTagsRoundTrip tag = tag == (parseNERtag . fromNERtag) tag
----
--prop_chunkTagsRoundTrip :: ChunkTag -> Bool
--prop_chunkTagsRoundTrip tag = tag == (parseChunkTag . fromChunkTag) tag

--test_toJson = assertEqual objVBG (toJSON (VBG))

--objVBG = String "VBG"
