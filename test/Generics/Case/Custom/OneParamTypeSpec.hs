{-# LANGUAGE DeriveGeneric #-}

module Generics.Case.Custom.OneParamTypeSpec (spec) where

import qualified GHC.Generics as G
import Generics.Case
import Generics.Chain
import qualified Generics.SOP as SOP
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

data OneParamType a
  = OPT1 a
  | OPT2 (Maybe a)
  | OPT3 a a
  deriving (Show, Eq, G.Generic)

instance SOP.Generic (OneParamType a)

instance (Q.Arbitrary a) => Q.Arbitrary (OneParamType a) where
  arbitrary =
    Q.oneof
      [ OPT1 <$> Q.arbitrary
      , OPT2 <$> Q.arbitrary
      , OPT3 <$> Q.arbitrary <*> Q.arbitrary
      ]
  shrink = Q.genericShrink

type OPTFn a r = (a -> r) -> (Maybe a -> r) -> (a -> a -> r) -> OneParamType a -> r

type FunArgs a r = '[Fun a r, Fun (Maybe a) r, Fun a (Fun a r), OneParamType a]

type OPTFun a r = Chain (FunArgs a r) r

manual :: OPTFn a r
manual fromA fromM fromAs = \case
  OPT1 a -> fromA a
  OPT2 m -> fromM m
  OPT3 a1 a2 -> fromAs a1 a2

optR :: forall a r. OPTFn a r
optR = gcaseR @(OneParamType a)

optL :: forall a r. OneParamType a -> (a -> r) -> (Maybe a -> r) -> (a -> a -> r) -> r
optL = gcaseL @(OneParamType a)

specOPT ::
  forall a r.
  ( Show a
  , Function a
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  OPTFn a r ->
  H.Spec
specOPT name f =
  specG @(FunArgs a r)
    ("manual", mkFn manual)
    (name, mkFn f)

mkFn ::
  OPTFn a r ->
  OPTFun a r
mkFn f f1 f2 f3 = f (applyFun f1) (applyFun f2) (applyFun <$> applyFun f3)

optL_ :: OPTFn a r
optL_ r fromInt fromStringChar opt = optL opt r fromInt fromStringChar

spec :: H.Spec
spec = do
  H.describe "OneParamType () -> Char" $ do
    specOPT @() @Char "optR" optR
    specOPT @() @Char "optL" optL_
  H.describe "OneParamType Char -> Either String ()" $ do
    specOPT @Char @(Either String ()) "optR" optR
    specOPT @Char @(Either String ()) "optL" optL_
  H.describe "OneParamType String -> (Int, Either Integer Int)" $ do
    specOPT @String @(Int, Either Integer Int) "optR" optR
    specOPT @String @(Int, Either Integer Int) "optL" optL_
  H.describe "OneParamType [Maybe (Int, String)] -> (Int, [Either (Maybe ()) String])" $ do
    specOPT @[Maybe (Int, String)] @(Int, [Either (Maybe ()) String]) "optR" optR
    specOPT @[Maybe (Int, String)] @(Int, [Either (Maybe ()) String]) "optL" optL_
