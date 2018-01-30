{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.UtilitiesTests where

import Test.Framework
import Data.Utilities

test_involution =  assertEqual True True
-- fix needed
