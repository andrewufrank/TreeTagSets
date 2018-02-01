{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for TinT parser for German
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

module  NLP.Corpora.German (module  NLP.Corpora.German
        )  where

import GHC.Generics
import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)


import  NLP.Types.Tags
import Data.Utilities

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
    Germanunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

spelledAs =
    [ (Dollarpoint, "$.")
    , (Dollaropenbracket, "$[")
    , (Dollarcomma, "$,")
    ]


instance POStags POStag where

    tagUNK = Germanunk

--    tagTerm = showTag

    startTag = START
    endTag = END

    isDeterminerTag tag = tag `elem` []  -- unknown what is a det here?
    tagMap = mkTagMap [minBound ..] spelledAs

instance Arbitrary POStag where
  arbitrary = elements [minBound ..]
instance Serialize POStag



