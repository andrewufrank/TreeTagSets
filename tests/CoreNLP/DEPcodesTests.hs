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

module CoreNLP.DEPcodesTests  where

import           Test.Framework

import CoreNLP.DEPcodes
import Data.Aeson
import GHC.Exts


test_1a = assertEqual (DepCode ACL Dep2Zero) (readDepCode "ACL")
test_2a = assertEqual (DepCode AUX ON) (readDepCode "AUX:ON")
test_2b = assertEqual (DepCode AUX Dep2Zero) (readDepCode "AUX")

test_3a = assertEqual "AUX:ON" (fromDEPtag $ DepCode AUX ON)
test_3b = assertEqual "AUX" (fromDEPtag $ DepCode AUX Dep2Zero)

test_depCode_cc = assertEqual (DepCode CC Dep2Zero) (readDepCode "CC")

test_toJson = assertEqual objAux (toJSON (DepCode AUX Dep2Zero))

objAux =
    Object
  (fromList
     [("tag", String "DepCode"), ("d1", String "AUX"),
      ("d2", String "Dep2Zero")])
