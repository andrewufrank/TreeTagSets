{-----------------------------------------------------------------------------
--
-- Module      :  Dependency code tests
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
--           MultiParamTypeClasses
--       , ScopedTypeVariables
--        , FlexibleContexts
     OverloadedStrings
--        , TypeSynonymInstances
--        , FlexibleInstances
--        , DeriveAnyClass
         #-}

module NLP.TagSets.DEPcodesTests  where

import           Test.Framework

import NLP.TagSets.DEPcodes
--import Data.Aeson
--import GHC.Exts
--import NLP.Tags

--instance CharChains2 DepCode Text where
instance Arbitrary DepCode1 where
  arbitrary = elements [minBound ..]
instance Arbitrary DepCode2 where
  arbitrary = elements [minBound ..]
--instance Zeros DepCode where zero = DepUnknown "constant zero"

--instance Arbitrary DepCode where
--    arbitrary = DepCode  d1 d2
--        where
--                d1 = arbitrary
--                d2 = arbitrary
--
--prop_tagsRoundTrip :: DepCode -> Bool
--prop_tagsRoundTrip tag = tag == (toDEPtag . fromDEPtag) tag

unkD =unkDEPtag :: DepCode
test_unk = assertEqual unkD (toDEPtag "xy99")
test_unk2 = assertEqual "DepUnk" (fromDEPtag unkD)

test_1a = assertEqual (DepCode ACL Dep2zero) (readDepCode "ACL")
test_2a = assertEqual (DepCode AUX ON) (readDepCode "AUX:ON")
test_2b = assertEqual (DepCode AUX Dep2zero) (readDepCode "AUX")

test_3a = assertEqual "AUX:ON" (fromDEPtag $ DepCode AUX ON)
test_3b = assertEqual "AUX" (fromDEPtag $ DepCode AUX Dep2zero)

test_depCode_cc = assertEqual (DepCode CC Dep2zero) (readDepCode "CC")

--test_toJson = assertEqual objAux (toJSON (DepCode AUX Dep2zero))

--objAux =
--    Object
--  (fromList
--     [("tag", String "DepCode"), ("d1", String "AUX"),
--      ("d2", String "Dep2zero")])
