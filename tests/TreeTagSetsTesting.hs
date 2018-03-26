-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework

import {-@ HTF_TESTS @-} Data.UtilitiesTests

import {-@ HTF_TESTS @-} NLP.TagSets.BrownTests
import {-@ HTF_TESTS @-} NLP.TagSets.ConllTests
import {-@ HTF_TESTS @-} NLP.TagSets.FrenchTests
import {-@ HTF_TESTS @-} NLP.TagSets.GermanTests
import {-@ HTF_TESTS @-} NLP.TagSets.SpanishTests
import {-@ HTF_TESTS @-} NLP.TagSets.ItalianTinTTests
import {-@ HTF_TESTS @-} NLP.TagSets.UDTests
import {-@ HTF_TESTS @-} NLP.TagSets.FrenchUDTests
import {-@ HTF_TESTS @-} NLP.TagSets.DEPcodesTests
import {-@ HTF_TESTS @-} NLP.TagSets.NERcodesTests


main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end TreeTagSetsTesting.hs test:\n"
                ++ show p)
    return ()

