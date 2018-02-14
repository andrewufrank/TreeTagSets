--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DefaultSignatures #-}
module NLP.Types.Tags (
        POStags (..)
        , NERtags (..)
        , ChunkTags (..)
        , TagsetIDs (..)
        , DEPtags (..)
        , Text
        ) where

import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readEither)

import Data.Utilities
import Data.Maybe

import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
import Test.QuickCheck.Instances ()

import qualified Data.Map as Map
import  Data.Map (Map (..))

--import Data.Utilities (ErrOrVal))

-- | The class of named entity sets.  This typeclass can be defined
-- entirely in terms of the required class constraints.
    -- removed Serialize and Bounded
class (Ord a, Eq a, Read a, Show a, Generic a) => NERtags a where
  fromNERtag :: a -> Text
  -- ^ convert Tag to the form used by the tagger
  fromNERtag = showT

  parseNERtag :: Text ->  a
  -- convert the tagger form to a type
  parseNERtag  = read2unk nerUNK
--  parseNERTag txt = toEitherErr $ readEither $ T.unpack txt

  nerUNK :: a
  -- ^ the value marking a tag which is not defined - always the last
  default nerUNK :: Bounded a => a
  nerUNK = maxBound

-- | The class of things that can be regarded as 'chunks'; Chunk tags
-- are much like POS tags, but should not be confused. Generally,
-- chunks distinguish between different phrasal categories (e.g.; Noun
-- Phrases, Verb Phrases, Prepositional Phrases, etc..)
class (Ord a, Eq a, Read a, Show a,   Generic a, Serialize a) => ChunkTags a where
  fromChunkTag :: a -> Text
  fromChunkTag = showT
  parseChunkTag :: Text -> a
  parseChunkTag   = read2unk notChunkTag
  notChunkTag :: a
  default notChunkTag :: Bounded a => a
  notChunkTag = maxBound

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
    tagTerm = fromTag
    startTag :: a
    endTag :: a
    -- | Check if a tag is a determiner tag.
    isDeterminerTag :: a -> Bool
    tagMap :: Map a Text

    fromTag a = fromMaybe (showT (tagUNK :: a) )
                $  Map.lookup a tagMap
--    tagUNK = UNKNOWN
    parseTag t = maybe tagUNK id $ Map.lookup t
            (reverseMap tagMap)

class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a) => DEPtags a where
    -- the dependency tags (as a single level, inclusive features?)
    fromDEPtag :: a -> Text
    parseDEPtag :: Text -> a
    tagDEPUNK :: a

    tagDEPmap :: Map a Text

    fromDEPtag a = maybe (showT (tagDEPUNK :: a) ) id
                $  Map.lookup a tagDEPmap
--    tagUNK = UNKNOWN
    parseDEPtag t = maybe tagDEPUNK id $ Map.lookup t
            (reverseMap tagDEPmap)

class TagsetIDs t where
    tagsetURL :: t ->  Text


