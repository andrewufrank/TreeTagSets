{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the POS tags and Chunk tags derived from
-- the Conll2000 training corpus.
module NLP.Corpora.Conll (
    module NLP.Corpora.Conll
    , POStags (..)
    ) where

--import Data.Serialize (Serialize)
--import qualified Data.Text as T
--import Data.Text (Text)
--import Text.Read (readEither)
--import Test.QuickCheck.Arbitrary (Arbitrary(..))
--import Test.QuickCheck.Gen (elements)

--import GHC.Generics

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
instance NERtags NERtag

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
    fromTag a = maybe "UNKNOWN" id $ Map.lookup a map3
    tagUNK = UNKNOWN
    parseTag t = maybe tagUNK id $ Map.lookup t reverseLabelMap
--  fromTag = showTag
--
--  parseTag txt = case readConllTag txt of
--                   Left  _ -> Unk
--                   Right t -> t
--
--  -- | Constant tag for "unknown"
--  tagUNK = Unk
--
--  tagTerm = showTag
--
--  startTag = START
--  endTag = END
--
--  isDt tag = tag `elem` [DT]

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]
--instance Serialize POStag

--readConllTag :: Text -> Either Error POStag
--readConllTag "#" = Right Hash
--readConllTag "$" = Right Dollar
--readConllTag "(" = Right Op_Paren
--readConllTag ")" = Right Cl_Paren
--readConllTag "''" = Right CloseDQuote
--readConllTag "``" = Right OpenDQuote
--readConllTag "," = Right Comma
--readConllTag "." = Right Term
--readConllTag ":" = Right Colon
--readConllTag txt = Right $ readTag2 tagTxtPatterns txt
----  let normalized = replaceAll tagTxtPatterns (T.toUpper txt)
----  in readOrErr normalized
--
---- | Order matters here: The patterns are replaced in reverse order
---- when generating tags, and in top-to-bottom when generating tags.
--tagTxtPatterns :: [(Text, Text)]
--tagTxtPatterns = [ ("$", "dollar")
--                 ]
--
--reversePatterns :: [(Text, Text)]
--reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns
--
--showTag :: POStag -> Text
--showTag Hash = "#"
--showTag Op_Paren = "("
--showTag Cl_Paren = ")"
--showTag CloseDQuote = "''"
--showTag OpenDQuote = "``"
--showTag Dollar = "$"
--showTag Comma = ","
--showTag Term = "."
--showTag Colon = ":"
--showTag tag = showTag2 tagTxtPatterns tag
----    replaceAll (reversePatterns tagTxtPatterns) (T.pack $ show tag)
--
----replaceAll :: [(Text, Text)] -> (Text -> Text)
----replaceAll patterns = foldl (.) id (map (uncurry T.replace) patterns)
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
         | Op_Paren -- ^ (
         | Cl_Paren -- ^ )
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
         | Unk
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

spelledAs =
         [  (ClosePar, ")"  )
         , (Colon,   ":"    )
         , ( Coma, ",")
         , ( Dash,  "--"   )
         , (Dollar , "$"    )

         , (LRB_,  "-LRB-")
        , (PRP_Dollar, "PRP$")
         , (Quotes , "''")
         , (Quotes2 , "``")
         , (WP_Dollar, "WP$")
         ] :: [(POStag,Text)]
