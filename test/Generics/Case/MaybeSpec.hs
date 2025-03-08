module Generics.Case.MaybeSpec (spec) where

import Generics.Case
import Generics.Chain
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

type MaybeFn a r = Maybe a -> r -> (a -> r) -> r

type FunArgs a r = '[Maybe a, r, Fun a r]

type MaybeFun a r = Chain (FunArgs a r) r

manual :: MaybeFn a r
manual Nothing r _ = r
manual (Just a) _ f = f a

specMaybe ::
  forall a r.
  ( Show a
  , Function a
  , Q.Arbitrary r
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Show r
  , Eq r
  ) =>
  String ->
  MaybeFn a r ->
  H.Spec
specMaybe name f =
  specG @(FunArgs a r)
    ("maybe", mkFn manual)
    (name, mkFn f)

mkFn ::
  MaybeFn a r ->
  MaybeFun a r
mkFn f m r fn = f m r (applyFun fn)

spec :: H.Spec
spec = do
  H.describe "Maybe () -> Char" $ do
    specMaybe @() @Char "maybeL" maybeL
  H.describe "Maybe Char -> Either String ()" $ do
    specMaybe @Char @(Either String ()) "maybeL" maybeL
  H.describe "Maybe String -> (Int, Either Integer Int)" $ do
    specMaybe @String @(Int, Either Integer Int) "maybeL" maybeL
  H.describe "Maybe [Maybe (Int, String)] -> (Int, [Either (Maybe ()) String])" $ do
    specMaybe @(Maybe (Int, String)) @(Int, [Either (Maybe ()) String]) "maybeL" maybeL
