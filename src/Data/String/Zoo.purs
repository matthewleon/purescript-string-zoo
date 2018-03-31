module Data.String.Zoo (
    AnimalString
  , Animal

  , fromSnake
  , fromLowerCamel
  , fromUpperCamel
  , fromTitle

  , toSnake
  , toLowerCamel
  , toUpperCamel
  , toTitle
  , toArray
) where

import Prelude

import Data.Array as A
import Data.Char as C
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as RE
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Zoo.LowerString (LowerString)
import Data.String.Zoo.LowerString as LS

data AnimalString = AnimalString Animal String
derive instance genericAnimalString :: Generic AnimalString _
instance showAnimalString :: Show AnimalString where
  show = genericShow

data Animal =
    Snake
  | LowerCamel
  | UpperCamel
  | Title
derive instance genericAnimal :: Generic Animal _
instance showAnimal :: Show Animal where
  show = genericShow

fromSnake :: String -> AnimalString
fromSnake = from Snake

fromLowerCamel :: String -> AnimalString
fromLowerCamel = from LowerCamel

fromUpperCamel :: String -> AnimalString
fromUpperCamel = from UpperCamel

fromTitle :: String -> AnimalString
fromTitle = from Title

toSnake :: AnimalString -> String
toSnake = to Snake

toLowerCamel :: AnimalString -> String
toLowerCamel = to LowerCamel

toUpperCamel :: AnimalString -> String
toUpperCamel = to UpperCamel

toTitle :: AnimalString -> String
toTitle = to Title

toArray :: AnimalString -> Array String
toArray = map LS.toString <<< process

from :: Animal -> String -> AnimalString
from = AnimalString

to :: Animal -> AnimalString -> String
to animal = toString animal <<< map LS.toString <<< process
  where
  toString Snake = S.joinWith "_"
  toString LowerCamel =
    A.uncons >>> case _ of
      (Just {head, tail}) -> head <> toString UpperCamel tail
      Nothing -> ""
  toString UpperCamel = S.joinWith "" <<< map capitalize
  toString Title = S.joinWith " " <<< map capitalize

  capitalize :: String -> String
  capitalize = S.uncons >>> case _ of
    Just {head, tail} -> (S.fromCharArray [C.toUpper head]) <> tail
    Nothing -> ""

process :: AnimalString -> Array LowerString
process (AnimalString animal s) = LS.fromString <$> split' animal s
  where
  split' :: Animal -> String -> Array String
  split' Snake = S.split (S.Pattern "_")
  split' LowerCamel =
    S.split (S.Pattern " ") <<< RE.replace (unsafeRegex "[A-Z]" global) " $&"
  split' UpperCamel = A.drop 1 <<< split' LowerCamel
  split' Title = S.split (S.Pattern " ")
