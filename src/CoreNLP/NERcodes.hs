{-----------------------------------------------------------------------------
--
-- Module      :  Dependency and other Codes
--
-- |
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
--       , ScopedTypeVariables
--        , FlexibleContexts
    , OverloadedStrings
--        , TypeSynonymInstances
--        , FlexibleInstances
--        , DeriveAnyClass
--        , DefaultSignatures
        , DeriveGeneric
        #-}

module CoreNLP.NERcodes (module CoreNLP.NERcodes
--    DepCode1(..), DepCode2 (..), DepCode
--        , isROOT, isPUNCT
----        , hasDepCode
--        , makeSimpleDepCode, makeDepCode
--        , Pos (..)  -- , Unk
--        , NERtag (..)
--        , isVerbCode, isNounCode, isPunctuation, isAdjective
--        , isClosedClass
--        , isSimpleCode
--        , isPOSpunctuation
--        , coarsePOS
--        , readDepCodes, showDepCodes
--        , SpeakerTag (..), readSpeakerTag
--        , Conll.Tag (..)

        )
         where

import           Test.Framework
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Utilities
import Text.Read (readEither)

--import              NLP.Corpora.Conll  hiding (NERtag (..))

-- | The class of named entity sets.  This typeclass can be defined
-- entirely in terms of the required class constraints.
    -- removed Serialize and Bounded
class (Ord a, Eq a, Read a, Show a ) => NERtags a where
  fromNERtag :: a -> Text
  -- ^ convert Tag to the form used by the tagger
  fromNERtag = showT
  -- does not reproduce exactly (I-LOC becomes I_LOC)

  fromNERtagNormalized :: a -> Text



  parseNERtag :: Text ->  a
  -- convert the tagger form to a type
--  parseNERtag  = read2unkF NERunk

  nerUNK :: a
  -- ^ the value marking a tag which is not defined - always the last
--  default nerUNK :: Bounded a => a

instance NERtags NERtag where
  parseNERtag txt = either (\_ -> NERunk txt) id (readEitherT txt2)
    where
        txt2 = T.replace "-" "_" txt
  nerUNK = UNK

  fromNERtagNormalized I_LOC  = fromNERtag LOC
  fromNERtagNormalized LUG = fromNERtag LOC

  fromNERtagNormalized I_ORG = fromNERtag ORG
  fromNERtagNormalized I_PER = fromNERtag PER
  fromNERtagNormalized PERS = fromNERtag PER
  fromNERtagNormalized I_MISC = fromNERtag MISC

parseNERtagList :: [Text] -> [NERtag]
parseNERtagList [] = []
parseNERtagList [a] = [parseNERtag a]
parseNERtagList (a:as) = parseNERtag a : map NERtagValue as

-- | Named entity categories defined for the Conll 2003 task.
data NERtag = PER
            | ORG
            | LOC
            | MISC
            | UNK
            -- found in Stanford CoreNLP 3.5.2
            -- Time, Location, Organization, Person, Money, Percent, Date
            | O
            | NUMBER
            | PERSON
            | DURATION
            | DATE
            | SET
            | TIME
            | ORDINAL
            | LOCATION
            | ORGANIZATION
            | MONEY
            | PERCENT
            -- for german
            | I_LOC
            | I_ORG
            | I_PER
            | I_MISC

            -- for spanish
            | PERS
            | LUG

            --
            | NERtagValue Text
            | NERunk Text

  deriving (Read, Show, Ord, Eq) -- ,  Enum, Bounded)

--instance Zeros NERtag where zero = NERunk

data SpeakerTag =  -- PER0 | PER1 | PER2 |
                    SpeakerNumber Text
                    | SpeakerName Text
                    | SpeakerValue Text

    deriving (Read, Show,  Ord, Eq)
    -- to encode the speaker tag -- any others? PER5 or 5 is seen

class (Ord a, Eq a, Read a, Show a ) => SpeakerTags a where
  fromSpeakerTag :: a -> Text
  -- ^ convert Tag to the form used by the tagger
  fromSpeakerTag = showT

  parseSpeakerTag :: Text ->  a
  -- convert the tagger form to a type
--  parseSpeakerTag  = read2unkF NERunk

  speakerUNK :: a
  -- ^ the value marking a tag which is not defined - always the last
--  default nerUNK :: Bounded a => a

instance SpeakerTags SpeakerTag where
    parseSpeakerTag t =  case (T.take 3 t) of
                "PER" -> SpeakerNumber (fromJust  $ T.stripPrefix "PER" t)
                _ -> SpeakerName t
    fromSpeakerTag (SpeakerNumber t) = "PER" <> t
    fromSpeakerTag (SpeakerName  t) = "Speaker" <> t

    speakerUNK = error "speaker unknown not expected"

parseSpeakerTagList :: [Text] -> [SpeakerTag]
parseSpeakerTagList [] = []
parseSpeakerTagList [a] = [parseSpeakerTag a]
parseSpeakerTagList (a:as) = parseSpeakerTag a : map SpeakerValue as

--instance CharChains2 SpeakerTag Text  where
------    show' PER0 = "PERO"
------    show' PER1 = "PER1"
------    show' PER2 = "PER2"
--    show' (Speaker n) = "Speaker " <> showT n

-- readSpeakerTag :: Text -> SpeakerTag
-- readSpeakerTag = readNoteT "readSpeakerTag"

--readSpeakerTag :: Text -> SpeakerTag
--readSpeakerTag  t = case t of
--                "PER0" -> PER0
--                "PER1" -> PER1
--                "PER2" -> PER2
--                s     -> Speaker s
--
--instance CharChains2 SpeakerTag Text  where
--    show' PER0 = "PERO"
--    show' PER1 = "PER1"
--    show' PER2 = "PER2"
--    show' (Speaker n) = "Speaker " <> showT n

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
