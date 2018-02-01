{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveAnyClass #-}
module NLP.Corpora.RawTags (
        RawPOStag (..)
        , RawNERtag (..)
        , RawChunk (..)
        , Text
        ) where

-- the raw tags are just newtypes of the text output from the tagger
-- raw tags are used to find all the tags used by a model

import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readEither)

import Data.Utilities

import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
import Test.QuickCheck.Instances ()

import qualified Data.Map as Map
import  Data.Map (Map (..))

import NLP.Types.Tags
--import Data.Utilities (ErrOrVal))

------------------------------------------------__N E R tags
newtype RawNERtag = RawNERtag Text
  deriving (Ord, Eq, Read, Show, Generic)

-- | POStags instance for unknown tagsets.
instance NERtags RawNERtag where
  fromNERtag (RawNERtag t) = t
  parseNERtag  = RawNERtag

  -- | Constant tag for "unknown"  -- cannot occur
  nerUNK = error "nerUNK cannot occur for RawNERtag"


instance Arbitrary RawNERtag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawNERtag $ T.pack str

------------------------------------------------------- P O S tags
-- | POStags instance for unknown tagsets.
newtype RawPOStag = RawPOStag Text
  deriving (Ord, Eq, Read, Show, Generic, Serialize)

instance POStags RawPOStag where
  fromTag (RawPOStag t) = t

  parseTag  = RawPOStag

  -- | Constant tag for "unknown"
  tagUNK = RawPOStag "Unk"

  tagTerm (RawPOStag t) = t

  startTag = RawPOStag "-START-"
  endTag = RawPOStag "-END-"

  isDeterminerTag (RawPOStag tg) = tg == "DT"
  tagMap = error "tagMap not implemented for RawPOStag"

instance Arbitrary RawPOStag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawPOStag $ T.pack str


-----------------------------------------------------------------C H U N K
-- | A fall-back 'ChunkTag' instance, analogous to 'RawTag'
newtype RawChunk = RawChunk Text
  deriving (Ord, Eq, Read, Show, Generic, Serialize)


instance ChunkTags RawChunk where
  fromChunkTag (RawChunk ch) = ch
  parseChunkTag txt =  RawChunk txt
  notChunkTag = RawChunk "O"

------------------------------------------------------------------- D E P tags

-- | A fallback Dependency tag instance.

newtype RawDEPtag = RawDEPtag Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawDEPtag

instance DEPtags RawDEPtag where
  fromDEPtag (RawDEPtag ch) = ch
  parseDEPtag txt =  RawDEPtag txt






