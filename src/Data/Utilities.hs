--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Data.Utilities (module Data.Utilities
    , Text
    , Map
--    , Arbitrary (..)
--    , Serialize
    ) where

-- a collection of utilities used in handling of tags



import qualified Data.Text as T
import Data.Text (Text)
--import qualified         Data.Char      as S          (isSpace, isLower, toLower, toUpper)
import Text.Read (readEither)
import           Test.Invariant
--import Test.Framework
--import Data.Text.Arbitrary
import qualified Data.Map as Map
import  Data.Map (Map (..))

type ErrOrVal a = Either Text a
-- ^ a type for recording errors as text

-- | Just a handy alias for Text
type Error = Text

(<>):: Text -> Text -> Text
(<>) = T.append

readEitherT :: Read a => Text -> Either Text a
readEitherT t = either (Left . s2t) Right  $ readEither . t2s $ t

read2unk :: Read a => a -> Text ->  a
-- ^ read a tag, if error report the first arg
-- could be readDef from safe package
read2unk unk t = case (readEither (t2s t)) of
                        Left msg -> unk
                        Right a ->   a

read2unkF :: Read a => (Text -> a) -> Text ->  a
-- ^ read a tag, if error report the first arg
-- could be readDef from safe package
read2unkF unk t = case (readEither (t2s t)) of
                        Left msg -> unk t
                        Right a ->   a

reverseMap :: (Ord b, Ord a) => Map a b -> Map b a
reverseMap m = Map.fromList [ (b,a) | (a,b) <- Map.assocs m]

reverseLookup :: Ord a => Map a Text -> Text -> Maybe a
reverseLookup m1 a1  = Map.lookup a1 (reverseMap m1)

showT :: Show a => a -> Text
showT = s2t . show

mkTagMap :: (Ord tag, Show tag) =>
        [tag] -> [(tag,Text)] -> Map tag Text
-- ^ produce the map with the written from and the tags
mkTagMap tags spellings = mapTag2text
    where

--        map1, map2, mapTag2text :: Map tag Text
        map1 = Map.fromList $ zip tags
            (map showT tags)
--
        map2 = Map.fromList spellings
        mapTag2text = Map.union map2 map1

mkTagMap4conv :: (Ord tag, Show tag) =>
        (Text -> Text) -> [tag] -> [(tag,Text)] -> Map tag Text
-- ^ produce the map with the written from and the tags
mkTagMap4conv conv tags spellings = mapTag2text
    where

--        map1, map2, mapTag2text :: Map tag Text
        map1 = Map.fromList $ zip tags
            (map (conv . showT) tags)
--
        map2 = Map.fromList spellings
        mapTag2text = Map.union map2 map1

--
s2t :: String -> Text
-- ^ String to Text (invertable)
s2t = T.pack

t2s :: Text -> String
-- ^ String to Text (invertable)
t2s = T.unpack

--toUpper' :: Text -> Text
--toUpper' = map T.toUpper
-- -- ^ convert all char to uppercase

--toUpperStart :: Text -> Text
---- ^ convert the first character to Uppercase - for  PosTags in Spanish
--toUpperStart t = (S.toUpper . T.head $ t ) `T.cons` (T.tail t)

splitIn2By :: Text -> Text -> Maybe (Text, Maybe Text)
-- split a text in two pieces, separated - if two are present
splitIn2By sep t = case T.splitOn sep t of
    [] -> Nothing
    [a] -> Just (a, Nothing)
    [a,b] -> Just (a, Just b)
    _ -> Nothing

flatMaybe :: Maybe (Maybe a) -> Maybe a
flatMaybe Nothing = Nothing
flatMaybe (Just (Just a)) = Just a
flatMaybe (Just Nothing)  = Nothing
