{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Data.Utilities (module Data.Utilities
    , Text
    , Map
    ) where

-- a collection of utilities used in handling of tags



import qualified Data.Text as T
import Data.Text (Text)
--import qualified         Data.Char      as S          (isSpace, isLower, toLower, toUpper)
--import Text.Read (readEither)
import           Test.Invariant
import Test.Framework
--import Data.Text.Arbitrary
import qualified Data.Map as Map
import  Data.Map (Map (..))

type ErrOrVal a = Either Text a
-- ^ a type for recording errors as text

-- | Just a handy alias for Text
type Error = Text

reverseMap :: (Ord b, Ord a) => Map a b -> Map b a
reverseMap m = Map.fromList [ (b,a) | (a,b) <- Map.assocs m]

--------------
--
--toEitherErr :: Either String a -> Either Error a
---- ^ convert a string error return to a text
---- better name toErrOrVal
--toEitherErr (Left s) = Left (T.pack s)
--toEitherErr (Right r) = Right r
--
--readOrErr :: Read a => Text -> Either Text a
--readOrErr    t = case (readEither (t2s t)) of
--                        Left msg -> Left (s2t msg)
--                        Right a -> Right a
--
--s2t :: String -> Text
---- ^ String to Text (invertable)
--s2t = T.pack
--
--t2s :: Text -> String
---- ^ String to Text (invertable)
--t2s = T.unpack

--toUpper' :: Text -> Text
--toUpper' = map T.toUpper
-- -- ^ convert all char to uppercase

--toUpperStart :: Text -> Text
---- ^ convert the first character to Uppercase - for  PosTags in Spanish
--toUpperStart t = (S.toUpper . T.head $ t ) `T.cons` (T.tail t)

