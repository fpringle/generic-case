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

type OPTFn a r = OneParamType a -> (a -> r) -> (Maybe a -> r) -> (a -> a -> r) -> r

type FunArgs a r = '[OneParamType a, Fun a r, Fun (Maybe a) r, Fun a (Fun a r)]

type OPTFun a r = Chain (FunArgs a r) r

manual :: OPTFn a r
manual opt fromA fromM fromAs = case opt of
  OPT1 a -> fromA a
  OPT2 m -> fromM m
  OPT3 a1 a2 -> fromAs a1 a2

gopt :: forall a r. OPTFn a r
gopt = gcase @(OneParamType a)

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
  forall a r.
  OPTFn a r ->
  OPTFun a r
mkFn f m f1 f2 f3 = f m (applyFun f1) (applyFun f2) (applyFun <$> applyFun f3)

spec :: H.Spec
spec = do
  H.describe "OneParamType () -> Char" $ do
    specOPT @() @Char "gopt" gopt
  H.describe "OneParamType Char -> Either String ()" $ do
    specOPT @Char @(Either String ()) "gopt" gopt
  H.describe "OneParamType String -> (Int, Either Integer Int)" $ do
    specOPT @String @(Int, Either Integer Int) "gopt" gopt
  H.describe "OneParamType [Maybe (Int, String)] -> (Int, [Either (Maybe ()) String])" $ do
    specOPT @[Maybe (Int, String)] @(Int, [Either (Maybe ()) String]) "gopt" gopt
