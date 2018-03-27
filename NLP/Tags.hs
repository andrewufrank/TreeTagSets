{-# LANGUAGE OverloadedStrings
    , ScopedTypeVariables
    , DefaultSignatures #-}

module NLP.Tags (
        POStags (..)
        , ChunkTags (..)
        , NERtags (..)
        , DEPtags (..)
        , TagsetIDs (..)
        , Text
        ) where

import Data.Text (Text)
import GHC.Generics

import Data.Utilities
import Data.Maybe

import qualified Data.Map as Map
import  Data.Map (Map (..))




-- | The class of things that can be regarded as 'chunks';
-- Chunk tags are much like POS tags, but should not be confused.
-- Generally, chunks distinguish between different phrasal categories (e.g.; Noun
-- Phrases, Verb Phrases, Prepositional Phrases, etc..)

class (Ord a, Eq a, Read a, Show a, Generic a) => ChunkTags a where
  {-# MINIMAL fromChunkTag, toChunkTag #-}
  fromChunkTag :: a -> Text
  fromChunkTag = showT

  toChunkTag :: Text -> a
--  toChunkTag   = read2unk notChunkTag

  unkChunkTag :: a
  default unkChunkTag :: Bounded a => a
  unkChunkTag = maxBound

-- | The class of POS Tags.
class (Ord a, Eq a, Read a, Show a, Generic a) => POStags a where
    {-# MINIMAL  mapPOStag
        | (fromPOStag, toPOStag) #-}
    fromPOStag :: a -> Text
    toPOStag :: Text -> a
    unkPOStag :: a
    default unkPOStag :: Bounded a => a
    unkPOStag = maxBound

    mapPOStag :: Map a Text
    mapPOStag = error "no mapPOStag implemented \
                    \ - used from and toPOStag"

    fromPOStag a = fromMaybe (showT (unkPOStag :: a) )
                $  Map.lookup a mapPOStag
    toPOStag t = maybe unkPOStag id $ Map.lookup t
            (reverseMap mapPOStag)


-- | The class for Dependency tags
class (Ord a, Eq a, Read a, Show a, Generic a) => DEPtags a where
    fromDEPtag :: a -> Text
    toDEPtag :: Text -> a

    unkDEPtag :: a

-- | The class of named entity sets.
class (Ord a, Eq a, Read a, Show a, Generic a) => NERtags a where
  fromNERtag :: a -> Text
  -- ^ convert Tag to the form used by the tagger
  fromNERtag = showT
  -- does not reproduce exactly (I-LOC becomes I_LOC)

  toNERtag :: Text ->  a
  -- convert the tagger form to a type
--  parseNERtag  = read2unkF NERunk

  unkNERtag :: a
  -- ^ the value marking a tag which is not defined - always the last
--  default nerUNK :: Bounded a => a

class (Ord a, Eq a, Read a, Show a ) => SpeakerTags a where
  fromSpeakerTag :: a -> Text
  -- ^ convert Tag to the form used by the tagger
  fromSpeakerTag = showT

  toSpeakerTag :: Text ->  a
  -- convert the tagger form to a type

  unkSpeakerTag :: a
  -- ^ the value marking a tag which is not defined - always the last

class TagsetIDs t where
    tagsetURL :: t ->  Text


