module Generics.Case.MaybeSpec (spec) where

import Generics.Case
import Generics.Chain
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

type MaybeFn a r = r -> (a -> r) -> Maybe a -> r

type FunArgs a r = '[r, Fun a r, Maybe a]

type MaybeFun a r = Chain (FunArgs a r) r

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
    ("maybe", mkFn maybe)
    (name, mkFn f)

mkFn ::
  MaybeFn a r ->
  MaybeFun a r
mkFn f r fn = f r (applyFun fn)

maybeL_ :: MaybeFn a r
maybeL_ x y b = maybeL b x y

spec :: H.Spec
spec = do
  H.describe "Maybe () -> Char" $ do
    specMaybe @() @Char "maybeR" maybeR
    specMaybe @() @Char "maybeL" maybeL_
  H.describe "Maybe Char -> Either String ()" $ do
    specMaybe @Char @(Either String ()) "maybeR" maybeR
    specMaybe @Char @(Either String ()) "maybeL" maybeL_
  H.describe "Maybe String -> (Int, Either Integer Int)" $ do
    specMaybe @String @(Int, Either Integer Int) "maybeR" maybeR
    specMaybe @String @(Int, Either Integer Int) "maybeL" maybeL_
  H.describe "Maybe [Maybe (Int, String)] -> (Int, [Either (Maybe ()) String])" $ do
    specMaybe @[Maybe (Int, String)] @(Int, [Either (Maybe ()) String]) "maybeR" maybeR
    specMaybe @(Maybe (Int, String)) @(Int, [Either (Maybe ()) String]) "maybeL" maybeL_
