{-----------------------------------------------------------------------------
--
-- Module      :  Test for speaker tags
--
-- |
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
        #-}

module NLP.TagSets.SpeakerTagsTests  where

import           Test.Framework
import NLP.TagSets.SpeakerTags
--import NLP.Tags

--test_listNull = assertEqual [NERunk text0] (parseNERtagList [text0])
--test_listOh = assertEqual [O] (parseNERtagList [textOh])
--test_unk = assertBool . isAnUnknownNER . toNERtag $ text0
--
--test_eq = assertBool $ [NERunk text0] == (parseNERtagList [text0])
--test_eqOh = assertBool $ [O] == (parseNERtagList [textOh])
--
--text0 = "0" :: Text
--textOh = "O" :: Text