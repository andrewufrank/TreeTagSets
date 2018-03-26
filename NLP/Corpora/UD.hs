{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for UD  -- the table is lifted
--
-----------------------------------------------------------------------------}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
--        , DeriveAnyClass
        , DeriveGeneric
        #-}

module NLP.Corpora.UD (module NLP.Corpora.UD
        , NLP.POStags(..)
        )
         where

import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import qualified NLP.Types.Tags as NLP
import Data.Utilities

undefUPOS = error "NLP.Corpora.UD" :: POStag

data POStag =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    ADJ | -- adjective
    ADP | -- adposition
    ADV | -- adverb
    AUX | -- auxiliary
    CCONJ | -- coordinating conjunction
    DET | -- determiner
    INTJ | -- interjection
    NOUN | -- noun
    NUM | -- numeral
    PART | -- particle
    PRON | -- pronoun
    PROPN | -- proper noun
    PUNCT | -- punctuation
    SCONJ | -- subordinating conjunction
    SYM | -- symbol
    VERB | -- verb
    X  -- other
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance NLP.TagsetIDs POStag where
    tagsetURL _ = "http://universaldependencies.org/u/pos/"

instance NLP.POStags  POStag where

    tagUNK = X

--    tagTerm = showTag

    startTag = START
    endTag = END

    isDeterminerTag tag = tag `elem` [DET]
    tagMap = mkTagMap [minBound ..] []

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]
instance Serialize POStag


