module Data.Case.EitherSpec (spec) where

import Data.Case
import Data.Chain
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

type EitherFn a b r = (a -> r) -> (b -> r) -> Either a b -> r

type FunArgs a b r = '[Fun a r, Fun b r, Either a b]

type EitherFun a b r = Chain (FunArgs a b r) r

specEither ::
  forall a b r.
  ( Show a
  , Function a
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Show b
  , Function b
  , Q.CoArbitrary b
  , Q.Arbitrary b
  , Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  EitherFn a b r ->
  H.Spec
specEither name f =
  specG @(FunArgs a b r)
    ("either", mkFn either)
    (name, mkFn f)

mkFn ::
  EitherFn a b r ->
  EitherFun a b r
mkFn e f g = e (applyFun f) (applyFun g)

eitherL_ :: EitherFn a b r
eitherL_ f g e = eitherL e f g

spec :: H.Spec
spec = do
  H.describe "Either () Char -> Char" $ do
    specEither @() @Char @Char "eitherR" eitherR
    specEither @() @Char @Char "eitherL" eitherL_
  H.describe "Either Char String -> Either String ()" $ do
    specEither @Char @String @(Either String ()) "eitherR" eitherR
    specEither @Char @String @(Either String ()) "eitherL" eitherL_
  H.describe "Either String (Maybe Integer) -> (Int, Either Integer Int)" $ do
    specEither @String @(Maybe Integer) @(Int, Either Integer Int) "eitherR" eitherR
    specEither @String @(Maybe Integer) @(Int, Either Integer Int) "eitherL" eitherL_
  H.describe "Either [Maybe (Int, String)] Int -> (Int, [Either (Maybe ()) String])" $ do
    specEither @[Maybe (Int, String)] @Int @(Int, [Either (Maybe ()) String]) "eitherR" eitherR
    specEither @(Maybe (Int, String)) @Int @(Int, [Either (Maybe ()) String]) "eitherL" eitherL_
