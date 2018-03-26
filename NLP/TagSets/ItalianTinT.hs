{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for TinT parser for italian
--name of pos tagset ISST TANL Tagset
-- from http://www.italianlp.it/docs/ISST-TANL-POStagset.pdf
-- model used TinT
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

module  NLP.TagSets.ItalianTinT (module  NLP.TagSets.ItalianTinT
        ) where

import GHC.Generics
--import Data.Serialize (Serialize)
import Data.Utilities
--import Test.QuickCheck.Arbitrary (Arbitrary(..))
--import Test.QuickCheck.Gen (elements)

import Data.Text   as T (replace)
import Text.Read (readEither)

import  NLP.Tags

undefTinTPos = undef "convertOneSnip2Triples postat TinT":: POStag

data POStag =   -- the definitions are in  http://www.italianlp.it/docs/ISST-TANL-POStagset.pdf
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    A         | --	felice
    AP         | --	nostro
    B         | --	domani
    BplusPC         | --	eccolo
    BN         | --	non
    CC         | --	e
    CS         | --	che
    DD         | --	quel
    DE         | --	che
    DI         | --	ogni
    DQ         | --	che
    DR         | --	cui
    E         | --	a
    EplusRD         | --	dalla
    EA      |
    FB         | --	 -
    FC         | --	 ;
    FF         | --	 -
    FS         | --	 ?
    I         | --	Oh
    N         | --	Sei
    NO         | --	ultima
    PC         | --	ti
    PCplusPC         | --	gliele
    PD         | --	quello
    PE         | --	Noi
    PI         | --	tutto
    PP         | --	mio
    PQ         | --	Che
    PR         | --	Che
    RD         | --	il   -- RD?
    RI         | --	una
    S         | --	nutrice
    SP         | --	FULVIA
    SW         | --	grand'
    T         | --	tutti
    V         | --	vedere
    VplusPC         | --	avervi
    VplusPCplusPC         | --	occuparsene
    VA         | --	 è
    VAplusPC         | --	averlo
    VM         | --	volevo
    VMplusPC         | --	poterci
    VMplusPCplusPC         | --	sferrarsene
    X         | --	FINE  -- residual class
    TinTunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

spelledAs =
    [     (VplusPC, "V+PC")
        , (VplusPCplusPC, "V+PC+PC")
        , (VAplusPC, "VA+PC")
        , (VMplusPC, "VM+PC")
        , (VMplusPCplusPC, "VM+PC+PC")
        , (EplusRD, "E+RD")
        , (BplusPC, "B+PC")
        , (PCplusPC, "PC+PC")
    ]


{- additional information available
     "index": 53,
          "word": "pregiudizi",
          "originalText": "pregiudizi",
          "lemma": "pregiudizio",
          "characterOffsetBegin": 267,
          "characterOffsetEnd": 277,
          "pos": "S",
          "featuresText": "Gender\u003dMasc|Number\u003dPlur",
          "ner": "O",
          "full_morpho": "pregiudizi pregiudizio+n+m+plur",
          "selected_morpho": "pregiudizio+n+m+plur",
          "guessed_lemma": false,
          "features": {
            "Gender": [
              "Masc"
            ],
            "Number": [
              "Plur"
            ]
            -}
instance POStags POStag where

    unkPOStag = TinTunk

--    tagTerm = showTag

--    startTag = START
--    endTag = END
--
--    isDeterminerTag tag = tag `elem` [RD]  -- unknown what is a det here?
    mapPOStag = mkTagMap [minBound ..] spelledAs

--instance Arbitrary POStag where
--  arbitrary = elements [minBound ..]
--instance Serialize POStag



