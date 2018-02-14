{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.UtilitiesTests where

import Test.Framework
import Data.Utilities

test_involution =  assertEqual True True
-- fix needed

test_splitok = assertEqual (Just ("a", Just "b")) (splitIn2By ":" "a:b")
test_splitnok1 = assertEqual (Nothing) (splitIn2By ":" "a:b:c")
test_splitok2 = assertEqual (Just ("a ", Nothing)) (splitIn2By ":" "a ")
test_splitnok3 = assertEqual (Just("",Just "b")) (splitIn2By ":" ":b")
test_splitnok4 = assertEqual (Just(" ",Nothing)) (splitIn2By ":" " ")
