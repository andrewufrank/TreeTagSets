{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
         #-}

module NLP.TagSets.RawTagsTests where

import Test.Framework

import  NLP.TagSets.RawTags

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)
import Data.Utilities

instance Arbitrary RawPOStag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawPOStag . s2t $ str

instance Arbitrary RawNERtag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawNERtag . s2t $ str

prop_tagsRoundTrip ::  RawPOStag -> Bool
prop_tagsRoundTrip tag = tag == (toPOStag . fromPOStag) tag

--prop_nerTagsRoundTrip :: NERtag -> Bool
--prop_nerTagsRoundTrip tag = tag == (parseNERtag . fromNERtag) tag
----
--prop_chunkTagsRoundTrip :: ChunkTag -> Bool
--prop_chunkTagsRoundTrip tag = tag == (parseChunkTag . fromChunkTag) tag

--test_toJson = assertEqual objVBG (toJSON (VBG))

--objVBG = String "VBG"
