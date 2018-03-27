{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for French parser for French
-- model is http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
-- pos tageset name is
-- from http://www.llf.cnrs.fr/Gens/Abeille/French-Treebank-fr.php
-- model is http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
-- called with -serverProperties StanfordCoreNLP-french-UD.properties
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

module NLP.TagSets.FrenchUD (module  NLP.TagSets.FrenchUD
 , module NLP.Tags
        )
         where

import GHC.Generics
import qualified Data.Text as T
import Data.Text (Text)
import Data.Utilities


import  NLP.Tags

undefFrenchUDPos = undef "convertOneSnip2Triples postat FrenchUD"::  POStag

data POStag =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    Dollarpoint | --    $.       |   --	0
    Dollaropenbracket | --  $[       |   --	 '
    Dollarcomma  |   --	,
    ADJA       |   --	environs
    ADJD       |   --	I.
    ADP |
    ADV       |   --	que
    APPO       |   --	l'épouse
    APPR       |   --	 --
    APPRART       |   --	 --
    APZR       |   --	avoir
    ART       |   --	DES
    CARD       |   --	XI
    FM       |   --	tous
    ITJ       |   --	oui
    KON       |   --	un
    KOUS       |   --	sous
    NE       |   --	XXII
    NOUN |
    NN       |   --	CONCLUSION
    PDAT       |   --	d'analyse
    PDS       |   --	une
    PIAT       |   --	ajouta
    PIDAT       |   --	jeune
    PIS       |   --	aller
    PPER       |   --	du
    PPOSAT       |   --	donner
    PRELS       |   --	qui
    PRF       |   --	café
    PRON |
    PROAV       |   --	d'un
    PTKANT       |   --	avec
    PTKNEG       |   --	net
    PTKVZ       |   --	fort
    PWAV       |   --	dit
    PWS       |   --	mon
    TRUNC       |   --	en
    VAFIN       |   --	C'est
    VAINF       |   --	sein
    VMFIN       |   --	démêlés
    VERB |
    VVFIN       |   --	chrétienne
    VVIMP       |   --	j'
    VVINF       |   --	bien
    VVIZU       |   --	hésitation
    VVPP       |   --	maintenant
    X |
    XY       |   --	n
    FrenchUnk  -- other
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

spelledAs =
    [ (Dollarpoint, "$.")
    , (Dollaropenbracket, "$[")
    , (Dollarcomma, "$,")
    ]

instance POStags POStag where
    unkPOStag = FrenchUnk
    mapPOStag = mkTagMap [minBound ..] spelledAs

