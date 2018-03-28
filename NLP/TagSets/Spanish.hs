{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for Spanish parser for spanish
--  tagset is defined https://nlp.stanford.edu/software/spanish-faq.shtml#tagset
-- tagset name is "simplified version of the tagset used in the AnCora"
-- model for stanford coreNLP is http://nlp.stanford.edu/software/stanford-spanish-corenlp-2017-06-09-models.jar

-- treetagger braucht https://www.sketchengine.co.uk/spanish-treetagger-part-of-speech-tagset/
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

module  NLP.TagSets.Spanish (module  NLP.TagSets.Spanish
 , module NLP.Tags
        )
         where

import GHC.Generics
--import Data.Serialize (Serialize)
import qualified Data.Map as Map
import  Data.Map (Map (..))
import Data.Maybe

import Data.Utilities
--import Test.QuickCheck.Arbitrary (Arbitrary(..))
--import Test.QuickCheck.Gen (elements)
import qualified Data.Text                as T


import  NLP.Tags

undefPOS = undef "convertOneSnip2Triples postat spanish":: POStag

data POStag =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
--    Adjectives
    Ao0000        |    -- Adjective (ordinal)	primera, segundo, últimos
    Aq0000        |    -- Adjective (descriptive)	populares, elegido, emocionada, andaluz
    Ap0000        |   -- ??
--    Conjunctions
    Cc        |    -- Conjunction (coordinating)	y, o, pero
    Cs        |    -- Conjunction (subordinating)	que, como, mientras
--    Determiners
    Da0000        |    -- Article (definite)	el, la, los, las
    Dd0000        |    -- Demonstrative	este, esta, esos
    De0000        |    -- "Exclamative" (TODO)	qué (¡Qué pobre!)
    Di0000        |    -- Article (indefinite)	un, muchos, todos, otros
    Dn0000        |    -- Numeral	tres, doscientas
    Do0000        |    -- Numeral (ordinal)	el 65 aniversario
    Dp0000        |    -- Possessive	sus, mi
    Dt0000        |    -- Interrogative	cuántos, qué, cuál
--    Punctuation
    F0        |    -- Other	&, @
    Faa        |    -- Inverted exclamation mark	¡
    Fat        |    -- Exclamation mark	!
    Fc        |    -- Comma	,
    Fca        |    -- Left bracket	[
    Fct        |    -- Right bracket	]
    Fd        |    -- Colon	:
    Fe        |    -- Double quote	"
    Fg        |    -- Hyphen	-
    Fh        |    -- Forward slash	/
    Fia        |    -- Inverted question mark	¿
    Fit        |    -- Question mark	?
    Fp        |    -- Period / full-stop	.
    Fpa        |    -- LeFt parenthesis	(
    Fpt        |    -- Right parenthesis	)
    Fra        |    -- LeFt guillemet / angle quote	«
    Frc        |    -- Right guillemet / angle quote	»
    Fs        |    -- Ellipsis	..., etcétera
    Ft        |    -- Percent sign	%
    Fx        |    -- Semicolon	;
    Fz        |    -- Single quote	'
--    Interjections
    I        |    -- Interjection	ay, ojalá, hola
--    Nouns
    Nc00000        |    -- Unknown common noun (neologism, loanword)	minidisc, hooligans, re-flotamiento
    Nc0c000  |  -- ??
    Nc0n000        |    -- Common noun (invariant number)	hipótesis, campus, golf
    Nc0p000        |    -- Common noun (plural)	años, elecciones
    Nc0s000        |    -- Common noun (singular)	lista, hotel, partido
    Np00000        |    -- Proper noun	Málaga, Parlamento, UFINSA
--    Pronouns
    P0000000        |    -- Impersonal se	se
    Pd000000        |    -- Demonstrative pronoun	éste, eso, aquellas
    Pe000000        |    -- "Exclamative" pronoun	qué
    Pi000000        |    -- Indefinite pronoun	muchos, uno, tanto, nadie
    Pn000000        |    -- Numeral pronoun	dos miles, ambos
    Pp000000        |    -- Personal pronoun	ellos, lo, la, nos
    Pr000000        |    -- Relative pronoun	que, quien, donde, cuales
    Pt000000        |    -- Interrogative pronoun	cómo, cuánto, qué
    Px000000        |    -- Possessive pronoun	tuyo, nuestra
--    Adverbs
    Rg        |    -- Adverb (general)	siempre, más, personalmente
    Rn        |    -- Adverb (negating)	no
--    Prepositions
    Sp000        |    -- Preposition	en, de, entre
--    Verbs
    Va00000        |    -- Verb (unknown)	should
    Vag0000        |    -- Verb (auxiliary, gerund)	habiendo
    Vaic000        |    -- Verb (auxiliary, indicative, conditional)	habría, habríamos
    Vaif000        |    -- Verb (auxiliary, indicative, future)	habrá, habremos
    Vaii000        |    -- Verb (auxiliary, indicative, imperfect)	había, habíamos
    Vaip000        |    -- Verb (auxiliary, indicative, present)	ha, hemos
    Vais000        |    -- Verb (auxiliary, indicative, preterite)	hubo, hubimos
    Vam0000        |    -- Verb (auxiliary, imperative)	haya
    Van0000        |    -- Verb (auxiliary, infinitive)	haber
    Vap0000        |    -- Verb (auxiliary, participle)	habido
    Vasi000        |    -- Verb (auxiliary, subjunctiVe, imperfect)	hubiera, hubiéramos, hubiese
    Vasp000        |    -- Verb (auxiliary, subjunctiVe, present)	haya, hayamos
    Vm00000  |  -- ?
    Vm0p000  |  -- ?
    Vmg0000        |    -- Verb (main, gerund)	dando, trabajando
    Vmi0000   |  -- ?
    Vmi2000  |  -- ?
    Vmic000        |    -- Verb (main, indicative, conditional)	daría, trabajaríamos
    Vmif000        |    -- Verb (main, indicative, future)	dará, trabajaremos
    Vmii000        |    -- Verb (main, indicative, imperfect)	daba, trabajábamos
    Vmim000   | -- ?
    Vmip000        |    -- Verb (main, indicative, present)	da, trabajamos
    Vmis000        |    -- Verb (main, indicative, preterite)	dio, trabajamos
    Vmm0000        |    -- Verb (main, imperatiVe)	da, dé, trabaja, trabajes, trabajemos
    Vmmp000  | -- ?
    Vmms000  | -- ?
    Vmn0000        |    -- Verb (main, infinitiVe)	dar, trabjar
    Vmp0000        |    -- Verb (main, participle)	dado, trabajado
    Vmsi000        |    -- Verb (main, subjunctiVe, imperfect)	diera, diese, trabajáramos, trabajésemos
    Vmsp000        |    -- Verb (main, subjunctiVe, present)	dé, trabajemos
    Vq00000     | -- ?
    Vs00000  |  -- ?
    Vsg0000        |    -- Verb (semiauxiliary, gerund)	siendo
    Vsic000        |    -- Verb (semiauxiliary, indicative, conditional)	sería, serían
    Vsif000        |    -- Verb (semiauxiliary, indicative, future)	será, seremos
    Vsii000        |    -- Verb (semiauxiliary, indicative, imperfect)	era, éramos
    Vsip000        |    -- Verb (semiauxiliary, indicative, present)	es, son
    Vsis000        |    -- Verb (semiauxiliary, indicative, preterite)	fue, fuiste
    Vsm0000        |    -- Verb (semiauxiliary, imperatiVe)	sea, sé
    Vsmp000   | -- ?
    Vsn0000        |    -- Verb (semiauxiliary, infinitiVe)	ser
    Vsp0000        |    -- Verb (semiauxiliary, participle)	sido
    Vssf000        |    -- Verb (semiauxiliary, subjunctiVe, future)	fuere
    Vssi000        |    -- Verb (semiauxiliary, subjunctiVe, imperfect)	fuera, fuese, fuéramos
    Vssp000        |    -- Verb (semiauxiliary, subjunctive, present)	sea, seamos
--    Dates
    W        |    -- Date	octubre, jueves, 2002
--    Numerals
    Z0        |    -- Numeral	547.000, 04, 52,52
    Zm        |    -- Numeral qualifier (currency)	dólares, euros
    Zu        |    -- Numeral qualifier (other units)	km, cc
--    Other
    Word        |    -- Emoticon or other symbol	:), ®
    Dollar   |  -- ?
    Posunk
--    Spanishunk  -- Text -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Enum, Bounded, Generic)

instance POStags POStag   where
    toPOStag "$" = Dollar
    toPOStag t = maybe Posunk  id
        $ Map.lookup t (reverseMap mapPOStag)

    fromPOStag Dollar = "$"
    fromPOStag a = maybe (showT (Posunk ) ) id
                $  Map.lookup a mapPOStag

    unkPOStag = Posunk
    mapPOStag = mkTagMap4conv toLowerStart [START ..] []

-- | Named entity categories defined for the Conll 2003 task.
    -- used in spanish?
data NERtag = PERS
            | LUG
            -- generic from Conll 2003 task - used in Spanish?
            |   PER
            | ORG
            | LOC
            | MISC
            | NERunk
    deriving (Show, Read, Eq, Ord, Generic)

instance NERtags NERtag where
  toNERtag txt = either (const NERunk) id (readEitherT txt)
  unkNERtag = NERunk
  fromNERtag = showT

