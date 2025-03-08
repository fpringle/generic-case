module Generics.Case.BoolSpec (spec) where

import Data.Bool
import Generics.Case
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Util

type BoolFn r = Bool -> r -> r -> r

type FunArgs r = '[Bool, r, r]

manual :: BoolFn r
manual b f t = bool f t b

specBool ::
  forall r.
  (Show r, Eq r, Q.Arbitrary r) =>
  String ->
  BoolFn r ->
  H.Spec
specBool name f = specG @(FunArgs r) ("bool", manual) (name, f)

spec :: H.Spec
spec = do
  H.describe "()" $ do
    specBool @() "boolL" boolL
  H.describe "Char" $ do
    specBool @Char "boolL" boolL
  H.describe "String" $ do
    specBool @String "boolL" boolL
  H.describe "[Maybe (Int, String)]" $ do
    specBool @[Maybe (Int, String)] "boolL" boolL
