module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Gen (unfoldable)
import Data.Array as A
import Data.Char as C
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Gen (genAlphaLowercaseString)
import Data.String.Zoo (
    fromSnake
  , fromLowerCamel
  , fromUpperCamel
  , fromTitle

  , toSnake
  , toLowerCamel
  , toUpperCamel
  , toTitle
  , toArray
)
import Test.QuickCheck (QC, quickCheckGen, (===))

main :: QC () Unit
main = do

  log "\nfromSnake"

  quickCheckStrs \strs ->
    toSnake (fromSnake $ snakify strs) === snakify strs

  quickCheckStrs \strs ->
    toLowerCamel (fromSnake $ snakify strs) === lowerCamelize strs

  quickCheckStrs \strs ->
    toUpperCamel (fromSnake $ snakify strs) === upperCamelize strs

  quickCheckStrs \strs ->
    toTitle (fromSnake $ snakify strs) === titlize strs

  quickCheckStrs \strs ->
    toArray (fromSnake $ snakify strs) === strs


  log "\nfromLowerCamel"

  quickCheckStrs \strs ->
    toSnake (fromLowerCamel $ lowerCamelize strs) === snakify strs

  quickCheckStrs \strs ->
    toLowerCamel (fromLowerCamel $ lowerCamelize strs) === lowerCamelize strs

  quickCheckStrs \strs ->
    toUpperCamel (fromLowerCamel $ lowerCamelize strs) === upperCamelize strs

  quickCheckStrs \strs ->
    toTitle (fromLowerCamel $ lowerCamelize strs) === titlize strs

  quickCheckStrs \strs ->
    toArray (fromLowerCamel $ lowerCamelize strs) === strs


  log "\nfromUpperCamel"

  quickCheckStrs \strs ->
    toSnake (fromUpperCamel $ upperCamelize strs) === snakify strs

  quickCheckStrs \strs ->
    toLowerCamel (fromUpperCamel $ upperCamelize strs) === lowerCamelize strs

  quickCheckStrs \strs ->
    toUpperCamel (fromUpperCamel $ upperCamelize strs) === upperCamelize strs

  quickCheckStrs \strs ->
    toTitle (fromUpperCamel $ upperCamelize strs) === titlize strs

  quickCheckStrs \strs ->
    toArray (fromUpperCamel $ upperCamelize strs) === strs


  log "\nfromTitle"

  quickCheckStrs \strs ->
    toSnake (fromTitle $ titlize strs) === snakify strs

  quickCheckStrs \strs ->
    toLowerCamel (fromTitle $ titlize strs) === lowerCamelize strs

  quickCheckStrs \strs ->
    toUpperCamel (fromTitle $ titlize strs) === upperCamelize strs

  quickCheckStrs \strs ->
    toTitle (fromTitle $ titlize strs) === titlize strs

  quickCheckStrs \strs ->
    toArray (fromTitle $ titlize strs) === strs

  where

  quickCheckStrs f = quickCheckGen $ f <$> unfoldable genAlphaLowercaseString

  snakify = S.joinWith "_"

  lowerCamelize =
    map S.toLower >>> A.uncons >>> case _ of
      (Just {head, tail}) -> head <> upperCamelize tail
      Nothing -> ""

  upperCamelize = map (S.toLower >>> capitalize) >>> S.joinWith ""

  titlize = map (S.toLower >>> capitalize) >>> S.joinWith " "

  capitalize = S.uncons >>> case _ of
    Just {head, tail} -> (S.fromCharArray [C.toUpper head]) <> tail
    Nothing -> ""
