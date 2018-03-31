module Data.String.Zoo.LowerString (
    LowerString
  , fromString
  , toString
  ) where

import Prelude

import Data.Monoid (class Monoid)
import Data.String (toLower)

newtype LowerString = LowerString String
derive newtype instance showLowerString :: Show LowerString
derive newtype instance semigroupLowerString :: Semigroup LowerString
derive newtype instance monoidLowerString :: Monoid LowerString

fromString :: String -> LowerString
fromString = LowerString <<< toLower

toString :: LowerString -> String
toString (LowerString string) = string
