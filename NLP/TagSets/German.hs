{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for   German
-- was also used in a french model from stanford coreNLP
--
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

module  NLP.TagSets.German (module  NLP.TagSets.German
 , module NLP.Tags
        )  where

import GHC.Generics

import  NLP.Tags
import Data.Utilities

undefGermanPos = undef "convertOneSnip2Triples postag german":: POStag

data POStag =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    Dollarpoint | --    $.       |   --	0
    Dollaropenbracket | --  $[       |   --	 '
    Dollarcomma  |   --	,
    ADJA       |   --	environs
    ADJD       |   --	I.
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
    VVFIN       |   --	chrétienne
    VVIMP       |   --	j'
    VVINF       |   --	bien
    VVIZU       |   --	hésitation
    VVPP       |   --	maintenant
    XY       |   --	n
    PTKZU |
    VAPP  |
    KOUI |
    PTKA |
    VMINF |
    VAIMP |
    PRELAT |
    PWAT |
    VMPP |
    PPOSS |
    KOKOM |
    GermanUnk -- Text -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

spelledAs =
    [ (Dollarpoint, "$.")
    , (Dollaropenbracket, "$[")
    , (Dollarcomma, "$,")
    ]


instance POStags POStag where

    unkPOStag = GermanUnk
    mapPOStag = mkTagMap [minBound ..] spelledAs

instance NERtags NERtag where
  toNERtag txt = either (const NERunk) id (readEitherT txt)
  unkNERtag = NERunk
  fromNERtag = showT

data NERtag = I_LOC
            | I_ORG
            | I_PER
            | I_MISC
            | NERunk

  deriving (Show, Read, Eq, Ord, Generic)

