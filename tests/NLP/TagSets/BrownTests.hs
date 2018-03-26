{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.TagSets.BrownTests where

import Test.Framework

import qualified NLP.TagSets.Brown as B
import  NLP.TagSets.Brown as B
import  NLP.TagSets.Brown
-- qualification here is not required

instance Arbitrary ChunkTag where
  arbitrary = elements [minBound ..]

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]

import  NLP.Tags

prop_tagsRoundTrip :: B.POStag -> Bool
prop_tagsRoundTrip tag = tag == (toPOStag . fromPOStag) tag

--test_brown1 :: Bool
test_brown1 = assertEqual ("WRB+DO"::Text)
                    (fromTag B.WRB_pl_DO :: Text)
test_brown2 = assertEqual (B.WRB_pl_DO::B.POStag)
                    (fromPOStag "WRB+DO" )

test_replace1 = assertEqual ("WRB_pl_DO")
       (replaceAll ( tagTxtPatterns) "WRB+DO")
test_read1 = assertEqual (Right WRB_pl_DO)
            (parseBrownTag "WRB+DO")
test_show1 = assertEqual "WRB+DO" (fromTag WRB_pl_DO)

--prop_nerTagsRoundTrip :: NERtag -> Bool
--prop_nerTagsRoundTrip tag = tag == (parseNERTag . fromNERTag) tag
--
prop_chunkTagsRoundTrip :: ChunkTag -> Bool
prop_chunkTagsRoundTrip tag = tag == (toChunkTag . fromChunkTag) tag
