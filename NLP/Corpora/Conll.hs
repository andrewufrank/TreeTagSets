{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the POS tags and Chunk tags derived from
-- the Conll2000 training corpus.
module NLP.Corpora.Conll (
    module NLP.Corpora.Conll
    , POStags (..)
    , POStag(..)
    ) where

import Data.Serialize (Serialize)
--import qualified Data.Text as T
--import Data.Text (Text)
--import Text.Read (readEither)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)
--import qualified Data.Map as Map

import GHC.Generics

--import qualified NLP.Types.Tags as NLP
import  NLP.Types.Tags  (NERtags (..), POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
import NLP.Types.General
import Data.Utilities
--import NLP.Types.Tree hiding (Chunk)
-- import NLP.Types.IOB

---- | Parse an IOB-formatted Conll corpus into TagagedSentences.
--parseTaggedSentences :: Text -> [TaggedSentence POStag]
--parseTaggedSentences rawCorpus =
--  let res :: Either Error [[IOBChunk Chunk POStag]]
--      res = parseIOB rawCorpus
--  in case res of
--       Left            err -> []
--       Right taggedCorpora -> map toTaggedSentence taggedCorpora

-- | Named entity categories defined for the Conll 2003 task.
data NERtag = PER
            | ORG
            | LOC
            | MISC
  deriving (Read, Show, Ord, Eq, Enum, Bounded)

instance Arbitrary NERtag where
  arbitrary = elements [minBound..]

--instance Serialize NERtag
--instance NERtags NERtag

instance TagsetIDs NERtag where
    tagsetURL _  = "Conll"

-- | Phrase chunk tags defined for the Conll task.
data Chunk = ADJP
           | ADVP
           | CONJP
           | INTJ
           | LST
           | NP -- ^ Noun Phrase.
           | PP -- ^ Prepositional Phrase.
           | PRT
           | SBAR
           | UCP
           | VP -- ^ Verb Phrase.
           | O -- ^ "out"; not a chunk.
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance Arbitrary Chunk where
  arbitrary = elements [minBound..]

instance Serialize Chunk


instance POStags POStag where
--    fromTag a = maybe (showT tagUNK) id $  Map.lookup a tagmap
----    tagUNK = UNKNOWN
--    parseTag t = maybe tagUNK id $ Map.lookup t (reverseMap tagmap)
--  fromTag = showTag
--
--  parseTag txt = case readConllTag txt of
--                   Left  _ -> Unk
--                   Right t -> t
--
  -- | Constant tag for "unknown"
    tagUNK = Unk

--
--  tagTerm = showTag
--
    startTag = START
    endTag = END
--
    isDt tag = tag `elem` [DT]

    tagMap = mkTagMap [minBound ..] spelledAs

--map1, map2, map3 :: Map POStag Text
--map1 = Map.fromList $ zip [minBound ..] (map showT [minBound .. maxBound :: POStag])
--
--map2 = Map.fromList spelledAs
---- show produces the "xx"
--
--map3 = Map.union map2 map1

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]
instance Serialize POStag


--
--instance ChunkTags Chunk where
--  fromChunk = T.pack . show
--  parseChunk txt =readOrErr  txt
--  notChunk = O

instance TagsetIDs POStag where
    tagsetURL _ = "https://en.wikipedia.org/wiki/Brown_Corpus#Part-of-speech_tags_used"
    -- this is a wiki page, anything better?

-- | These tags may actually be the Penn Treebank tags.  But I have
-- not (yet?) seen the punctuation tags added to the Penn set.
--
-- This particular list was complied from the union of:
--
--   * All tags used on the Conll2000 training corpus. (contributing the punctuation tags)
--   * The PennTreebank tags, listed here: <https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html> (which contributed LS over the items in the corpus).
--   * The tags: START, END, and Unk, which are used by Chatter.
--
data POStag = START -- ^ START tag, used in training.
         | END -- ^ END tag, used in training.
         | Hash -- ^ #
         | Dollar -- ^ $
         | CloseDQuote -- ^ ''
         | OpenDQuote -- ^ ``
         | OpParen -- ^ (
         | ClParen -- ^ )
         | Comma -- ^ ,
         | Term -- ^ . Sentence Terminator
         | Colon -- ^ :

         | CC -- ^ Coordinating conjunction
         | CD -- ^ Cardinal number
         | DT -- ^ Determiner
         | EX -- ^ Existential there
         | FW -- ^ Foreign word
         | IN -- ^ Preposition or subordinating conjunction
         | JJ -- ^ Adjective
         | JJR -- ^ Adjective, comparative
         | JJS -- ^ Adjective, superlative
         | LS -- ^ List item marker
         | MD -- ^ Modal
         | NN -- ^ Noun, singular or mass
         | NNS -- ^ Noun, plural
         | NNP -- ^ Proper noun, singular
         | NNPS -- ^ Proper noun, plural
         | PDT -- ^ Predeterminer
         | POS -- ^ Possessive ending
         | PRP -- ^ Personal pronoun
         | PRPdollar -- ^ Possessive pronoun
         | RB -- ^ Adverb
         | RBR -- ^ Adverb, comparative
         | RBS -- ^ Adverb, superlative
         | RP -- ^ Particle
         | SYM -- ^ Symbol
         | TO -- ^ to
         | UH -- ^ Interjection
         | VB -- ^ Verb, base form
         | VBD -- ^ Verb, past tense
         | VBG -- ^ Verb, gerund or present participle
         | VBN -- ^ Verb, past participle
         | VBP -- ^ Verb, non-3rd person singular present
         | VBZ -- ^ Verb, 3rd person singular present
         | WDT -- ^ Wh-determiner
         | WP -- ^ Wh-pronoun
         | WPdollar -- ^ Possessive wh-pronoun
         | WRB -- ^ Wh-adverb
         -- additions - af
         | LRB --
         | RRB
         | Dash
         | Unk
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

spelledAs =
         [  (ClParen, ")"  )
         ,(OpParen, "("  )
         , (Colon,   ":"    )
         , ( Comma, ",")
         , ( Term, ".")
         , ( Dash,  "--"   )
--         , (Dollar , "$"    )

         , (LRB,  "-LRB-")
         , (RRB,  "-RRB-")
        , (PRPdollar, "PRP$")
         , (CloseDQuote , "''")
         , (OpenDQuote , "``")
         , (WPdollar, "WP$")
         ] :: [(POStag,Text)]
