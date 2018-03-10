{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
         #-}

module NLP.Corpora.ConllTests where

import Test.Framework


import  NLP.Types.Tags

import  NLP.Corpora.Conll
import  NLP.Corpora.ConllNER
import NLP.Corpora.ConllChunks

import Data.Aeson
import GHC.Exts

prop_tagsRoundTrip ::  POStag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

--prop_nerTagsRoundTrip :: NERtag -> Bool
--prop_nerTagsRoundTrip tag = tag == (parseNERtag . fromNERtag) tag
----
--prop_chunkTagsRoundTrip :: ChunkTag -> Bool
--prop_chunkTagsRoundTrip tag = tag == (parseChunkTag . fromChunkTag) tag

test_toJson = assertEqual objVBG (toJSON (VBG))

objVBG = String "VBG"
