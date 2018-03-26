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

--import {-@ HTF_TESTS @-} NLP.Corpora.BrownTests
--import {-@ HTF_TESTS @-} NLP.Corpora.ConllTests
--import {-@ HTF_TESTS @-} NLP.Corpora.FrenchTests
--import {-@ HTF_TESTS @-} NLP.Corpora.GermanTests
--import {-@ HTF_TESTS @-} NLP.Corpora.SpanishTests
--import {-@ HTF_TESTS @-} NLP.Corpora.ItalianTinTTests
--import {-@ HTF_TESTS @-} NLP.Corpora.UDTests
--import {-@ HTF_TESTS @-} NLP.Corpora.FrenchUDTests
--import {-@ HTF_TESTS @-} CoreNLP.DEPcodesTests
import {-@ HTF_TESTS @-} CoreNLP.NERcodesTests


main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end TreeTagSetsTesting.hs test:\n"
                ++ show p)
    return ()

