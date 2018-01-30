{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Corpora.BrownTests where

import Test.Framework

import qualified NLP.Corpora.Brown as B
import  NLP.Corpora.Brown
-- qualification here is not required

import  NLP.Types.Tags

prop_tagsRoundTrip :: B.POStag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

--test_brown1 :: Bool
test_brown1 = assertEqual ("WRB+DO"::Text)
                    (fromTag B.WRB_pl_DO :: Text)
test_brown2 = assertEqual (B.WRB_pl_DO::B.POStag)
                    (parseTag "WRB+DO" )

test_replace1 = assertEqual ("WRB_pl_DO")
       (replaceAll ( tagTxtPatterns) "WRB+DO")
test_read1 = assertEqual (Right WRB_pl_DO)
            (parseBrownTag "WRB+DO")
test_show1 = assertEqual "WRB+DO" (fromTag WRB_pl_DO)
