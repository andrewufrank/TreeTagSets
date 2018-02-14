{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for French parser for French
-- model is http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
-- pos tageset name is
-- from http://www.llf.cnrs.fr/Gens/Abeille/French-Treebank-fr.php
http://www.llf.cnrs.fr/sites/sandbox.linguist.univ-paris-diderot.fr/files/statiques/french_treebank/guide-morpho-synt.02.pdf
-- model is http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
with set to -serverProperties StanfordCoreNLP-french.properties
-----------------------------------------------------------------------------}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
        , DeriveGeneric
        #-}

module NLP.Corpora.French (module NLP.Corpora.French
        , POStag (..)
        )
         where

import GHC.Generics
import Data.Serialize (Serialize)
import Data.Utilities
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import  NLP.Types.Tags  (POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
import Data.Utilities

data POStag =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    DET |
    N |
    P |
    NPP |
    PUNC |
    ET |
    NC |
    ADJ |
    ADV |  -- found in output, example peu (UD code set)
    CLS |
    V |
    VPR |
    VINF |
    CLR |
    VPP |
    PRO |
    CC |
    CS |
    PROREL |
    C |
    PREF |
    CLO |
    I |
    ADVWH |
    VIMP |
    DETWH |
    ADJWH |
    CL |
    PROWH |
    VS |
    Frenchunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance POStags POStag where

    tagUNK = Frenchunk

--    tagTerm = showTag

    startTag = START
    endTag = END

    isDeterminerTag tag = tag `elem` []  -- unknown what is a det here?
    tagMap = mkTagMap [minBound ..] []

instance Arbitrary POStag  where
  arbitrary = elements [minBound ..]
instance Serialize POStag

