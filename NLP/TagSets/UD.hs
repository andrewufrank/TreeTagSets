{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for UD  -- the table is lifted
-- missing he feature codes (which is requiring some work!)
--
-----------------------------------------------------------------------------}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
--        , DeriveAnyClass
        , DeriveGeneric
        #-}

module NLP.TagSets.UD (module NLP.TagSets.UD
 , module NLP.Tags
        )
         where

import GHC.Generics
import  NLP.Tags
import Data.Utilities

undefPOS = error "NLP.TagSets.UD" :: POStag

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

instance  TagsetIDs POStag where
    tagsetURL _ = "http://universaldependencies.org/u/pos/"

instance  POStags  POStag where

    unkPOStag = X

    mapPOStag = mkTagMap [minBound ..] []


