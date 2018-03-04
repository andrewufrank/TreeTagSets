{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the POS tags and Chunk tags derived from
-- the Conll2000 training corpus.
    -- it is an attempt to a complete list of penn treebank codes
    -- see http://erwinkomen.ruhosting.nl/eng/2014_Longdale-Labels.htm and
    -- https://stackoverflow.com/questions/1833252/java-stanford-nlp-part-of-speech-labels

module NLP.Corpora.Conll (
    module NLP.Corpora.Conll
    , POStags (..)
    , POStag(..)
    ) where

import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import  NLP.Types.Tags  (POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
--import NLP.Types.General
import Data.Utilities
-- import Uniform.Error (undef)

--undef :: String -> a
--undef = error
---- ^ for type specification, not to be evaluated

undefConll = error "convertOneSnip2Triples postag conll":: POStag

---- | Named entity categories defined for the Conll 2003 task.
--data NERtag = PER
--            | ORG
--            | LOC
--            | MISC
--  deriving (Read, Show, Ord, Eq, Enum, Bounded)
--
--instance Arbitrary NERtag where
--  arbitrary = elements [minBound..]

--instance Serialize NERtag
--instance NERtags NERtag


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
-- | Constant tag for "unknown"
    tagUNK = Unk

--
--  tagTerm = showTag
--
    startTag = START
    endTag = END
--
    isDeterminerTag tag = tag `elem` [DT]

    tagMap = mkTagMap [minBound ..] spelledAs


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
         , ( Hash,  "#"   )
--         , (Dollar , "$"    )

         , (LRB,  "-LRB-")
         , (RRB,  "-RRB-")
        , (PRPdollar, "PRP$")
         , (CloseDQuote , "''")
         , (OpenDQuote , "``")
         , (WPdollar, "WP$")
         ] :: [(POStag,Text)]
