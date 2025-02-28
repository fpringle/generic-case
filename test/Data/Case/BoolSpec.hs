module Data.Case.BoolSpec (spec) where

import Data.Bool
import Data.Case
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Util

specBool ::
  forall a.
  (Show a, Eq a, Q.Arbitrary a) =>
  String ->
  (a -> a -> Bool -> a) ->
  H.Spec
specBool name f = specG @'[a, a, Bool] ("bool", bool) (name, f)

boolL_ :: a -> a -> Bool -> a
boolL_ x y b = boolL b x y

spec :: H.Spec
spec = do
  H.describe "()" $ do
    specBool @() "boolR" boolR
    specBool @() "boolL" boolL_
  H.describe "Char" $ do
    specBool @Char "boolR" boolR
    specBool @Char "boolL" boolL_
  H.describe "String" $ do
    specBool @String "boolR" boolR
    specBool @String "boolL" boolL_
  H.describe "[Maybe (Int, String)]" $ do
    specBool @[Maybe (Int, String)] "boolR" boolR
    specBool @[Maybe (Int, String)] "boolL" boolL_
