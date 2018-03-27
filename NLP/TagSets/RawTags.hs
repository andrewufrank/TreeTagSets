{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveAnyClass #-}

module NLP.TagSets.RawTags (
        module NLP.TagSets.RawTags
         , module NLP.Tags
        ) where

-- the raw tags are just newtypes of the text output from the tagger
-- raw tags are used to find all the tags used by a model
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readEither)

import Data.Utilities

import qualified Data.Map as Map
import  Data.Map (Map (..))

import NLP.Tags


------------------------------------------------------- P O S tags
-- | POStags instance for unknown tagsets.
newtype RawPOStag = RawPOStag Text
  deriving (Ord, Eq, Read, Show, Generic)

instance POStags RawPOStag where
  fromPOStag (RawPOStag t) = t

  toPOStag  = RawPOStag

  -- | Constant tag for "unknown"
  -- should not occur, as all tags are mapped
  unkPOStag = RawPOStag "Unk"

  mapPOStag = error "tagMap not implemented for RawPOStag"

--instance Arbitrary RawPOStag where
--  arbitrary = do
--    NonEmpty str <- arbitrary
--    return $ RawPOStag $ T.pack str


-----------------------------------------------------------------C H U N K
-- | A fall-back 'ChunkTag' instance, analogous to 'RawTag'
newtype RawChunk = RawChunk Text
  deriving (Ord, Eq, Read, Show, Generic)


instance ChunkTags RawChunk where
  fromChunkTag (RawChunk ch) = ch
  toChunkTag txt =  RawChunk txt
  unkChunkTag = RawChunk "O"


------------------------------------------------------------------- D E P tags

-- | A fallback Dependency tag instance.

newtype RawDEPtag = RawDEPtag Text
  deriving (Show, Read, Eq, Ord, Generic)

--instance Serialize RawDEPtag

instance DEPtags RawDEPtag where
  fromDEPtag (RawDEPtag ch) = ch
  toDEPtag txt =  RawDEPtag txt
  unkDEPtag  = error "not implemented notDEPtag" --  DepCode DepUnk Dep2zero
--  notDEPtag = error "not implemented notDEPtag"

------------------------------------------------__N E R tags
newtype RawNERtag = RawNERtag Text
  deriving (Show, Read, Eq, Ord,  Generic)

-- | POStags instance for unknown tagsets.
instance NERtags RawNERtag where
  fromNERtag (RawNERtag t) = t
  toNERtag  = RawNERtag

  unkNERtag = error "nerUNK cannot occur for RawNERtag"






