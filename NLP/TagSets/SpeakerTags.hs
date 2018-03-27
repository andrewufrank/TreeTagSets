{-----------------------------------------------------------------------------
--
-- Module      :  Speaker Tags
--
-- | the tags are different in different models
-- an attempt to systematize
--
-----------------------------------------------------------------------------}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
--       , ScopedTypeVariables
--        , FlexibleContexts
    , OverloadedStrings
--        , TypeSynonymInstances
--        , FlexibleInstances
        , DeriveAnyClass
--        , DefaultSignatures
        , DeriveGeneric
        #-}

module NLP.TagSets.SpeakerTags (module NLP.TagSets.SpeakerTags
 , module NLP.Tags
        )
         where
import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Utilities
import Text.Read (readEither)
import GHC.Generics
import NLP.Tags


data SpeakerTag =  -- PER0 | PER1 | PER2 |
                    SpeakerNumber Text
                    | SpeakerName Text
                    | SpeakerValue Text

    deriving (Show, Read, Eq, Ord,  Generic)
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

