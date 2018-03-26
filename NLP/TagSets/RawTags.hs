{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveAnyClass #-}
module NLP.TagSets.RawTags (
        RawPOStag (..)
 , module NLP.Tags
        ) where

-- the raw tags are just newtypes of the text output from the tagger
-- raw tags are used to find all the tags used by a model

--import Data.Serialize (Serialize)
--import Data.Serialize.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readEither)

import Data.Utilities

--import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
--import Test.QuickCheck.Instances ()

import qualified Data.Map as Map
import  Data.Map (Map (..))

import NLP.Tags
--import Data.Utilities (ErrOrVal))


------------------------------------------------------- P O S tags
-- | POStags instance for unknown tagsets.
newtype RawPOStag = RawPOStag Text
  deriving (Ord, Eq, Read, Show, Generic)

instance POStags RawPOStag where
  fromPOStag (RawPOStag t) = t

  toPOStag  = RawPOStag

  -- | Constant tag for "unknown"
  unkPOStag = RawPOStag "Unk"

--  tagTerm (RawPOStag t) = t

--  startTag = RawPOStag "-START-"
--  endTag = RawPOStag "-END-"
--
--  isDeterminerTag (RawPOStag tg) = tg == "DT"
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







