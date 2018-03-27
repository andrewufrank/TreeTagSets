{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.TagSets.BrownTests where

import Test.Framework

import  NLP.TagSets.Brown as B
import  NLP.TagSets.Brown
-- qualification here is not required

instance Arbitrary ChunkTag where
  arbitrary = elements [minBound ..]

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]


prop_tagsRoundTrip :: B.POStag -> Bool
prop_tagsRoundTrip tag = tag == (toPOStag . fromPOStag) tag

--test_brown1 :: Bool
test_brown1 = assertEqual ("WRB+DO")
                    (fromPOStag B.WRB_pl_DO  )
test_brown2 = assertEqual (B.WRB_pl_DO::B.POStag)
                    (toPOStag "WRB+DO" )

test_replace1 = assertEqual ("WRB_pl_DO")
       (replaceAll ( tagTxtPatterns) "WRB+DO")
test_read1 = assertEqual (Right WRB_pl_DO)
            (parseBrownTag "WRB+DO")
test_show1 = assertEqual "WRB+DO" (fromPOStag WRB_pl_DO)

unkP =unkPOStag :: B.POStag
test_unk = assertEqual unkP (toPOStag "xy99")
test_unk2 = assertEqual "Unk" (fromPOStag unkP)

prop_chunkTagsRoundTrip :: ChunkTag -> Bool
prop_chunkTagsRoundTrip tag = tag == (toChunkTag . fromChunkTag) tag
