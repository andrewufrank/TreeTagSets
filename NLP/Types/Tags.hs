{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module NLP.Types.Tags (
        POStags (..)
        , NERtags (..)
        , ChunkTags (..)
        , TagsetIDs (..)
        , Text
        ) where

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

--import Data.Utilities (ErrOrVal))

-- | The class of named entity sets.  This typeclass can be defined
-- entirely in terms of the required class constraints.
class (Ord a, Eq a, Read a, Show a, Bounded a, Generic a, Serialize a) => NERtags a where
  fromNERTag :: a -> Text
  fromNERTag = T.pack . show

  parseNERTag :: Text ->  a
  parseNERTag txt = read2unk nerUNK txt
--  parseNERTag txt = toEitherErr $ readEither $ T.unpack txt
  nerUNK :: a
  nerUNK = maxBound

-- | The class of things that can be regarded as 'chunks'; Chunk tags
-- are much like POS tags, but should not be confused. Generally,
-- chunks distinguish between different phrasal categories (e.g.; Noun
-- Phrases, Verb Phrases, Prepositional Phrases, etc..)
class (Ord a, Eq a, Read a, Show a, Bounded a, Generic a, Serialize a) => ChunkTags a where
  fromChunkTag :: a -> Text
  fromChunkTag = showT
  parseChunkTag :: Text -> a
  parseChunkTag txt = read2unk notChunk txt
  notChunk :: a
  notChunk = maxBound

-- | The class of POS Tags.
--
-- We use a typeclass here because POS tags just need a few things in
-- excess of equality (they also need to be serializable and human
-- readable).  Passing around all the constraints everywhere becomes a
-- hassle, and it's handy to have a uniform interface to the diferent
-- kinds of tag types.
--
-- This typeclass also allows for corpus-specific tags to be
-- distinguished; They have different semantics, so they should not be
-- merged.  That said, if you wish to create a unifying POS Tag set,
-- and mappings into that set, you can use the type system to ensure
-- that that is done correctly.
--
-- This /may/ get renamed to POSTag at some later date.
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a) => POStags a where
    fromTag :: a -> Text
    parseTag :: Text -> a
    tagUNK :: a
    tagTerm :: a -> Text
    startTag :: a
    endTag :: a
    -- | Check if a tag is a determiner tag.
    isDt :: a -> Bool
    tagMap :: Map a Text

    fromTag a = maybe (showT (tagUNK :: a) ) id
                $  Map.lookup a tagMap
--    tagUNK = UNKNOWN
    parseTag t = maybe tagUNK id $ Map.lookup t
            (reverseMap tagMap)

--    -- lower level default implementations
--    showTag2 :: [(Text, Text)] -> a -> Text
----    showTag2 tagTxtPatterns tag = replaceAll (reversePatterns tagTxtPatterns) (s2t $ show tag)
--
--    readTag2 :: [(Text, Text)] -> Text -> a
----    readTag2 tagTxtPatterns = either (return tagUNK) id . readOrErr . normalized tagTxtPatterns

--    normalized :: [(Text, Text)] ->  Text -> a
--    normalized tagTxtPatterns = replaceAll tagTxtPatterns (T.toUpper txt)

class TagsetIDs t where
    tagsetURL :: t ->  Text

---- | A fall-back 'ChunkTag' instance, analogous to 'RawTag'
--newtype RawChunk = RawChunk Text
--  deriving (Ord, Eq, Read, Show, Generic)
--
--instance Serialize RawChunk
--
--instance ChunkTags RawChunk where
--  fromChunk (RawChunk ch) = ch
--  parseChunk txt =  (RawChunk txt)
--  notChunk = RawChunk "O"
--
---- | A fallback POS tag instance.
--newtype RawTag = RawTag Text
--  deriving (Ord, Eq, Read, Show, Generic)
--
--instance Serialize RawTag
--
---- | POStags instance for unknown tagsets.
--instance POStags RawTag where
--  fromTag (RawTag t) = t
--
--  parseTag t = RawTag t
--
--  -- | Constant tag for "unknown"
--  tagUNK = RawTag "Unk"
--
--  tagTerm (RawTag t) = t
--
--  startTag = RawTag "-START-"
--  endTag = RawTag "-END-"
--
--  isDt (RawTag tg) = tg == "DT"
--  tagMap = error "tagMap not implemented for RawTag"
--
--instance Arbitrary RawTag where
--  arbitrary = do
--    NonEmpty str <- arbitrary
--    return $ RawTag $ T.pack str
