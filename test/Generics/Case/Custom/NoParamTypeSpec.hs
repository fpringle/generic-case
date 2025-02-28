{-# LANGUAGE DeriveGeneric #-}

module Generics.Case.Custom.NoParamTypeSpec (spec) where

import qualified GHC.Generics as G
import Generics.Case
import Generics.Chain
import qualified Generics.SOP as SOP
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

data NoParamType
  = NPT1
  | NPT2 Int
  | NPT3 String Char
  deriving (Show, Eq, G.Generic)

instance SOP.Generic NoParamType

instance Q.Arbitrary NoParamType where
  arbitrary =
    Q.oneof
      [ pure NPT1
      , NPT2 <$> Q.arbitrary
      , NPT3 <$> Q.arbitrary <*> Q.arbitrary
      ]
  shrink = Q.genericShrink

type NPTFn r = r -> (Int -> r) -> (String -> Char -> r) -> NoParamType -> r

type FunArgs r = '[r, Fun Int r, Fun String (Fun Char r), NoParamType]

type NPTFun r = Chain (FunArgs r) r

manual :: NPTFn r
manual r fromInt fromStringChar = \case
  NPT1 -> r
  NPT2 int -> fromInt int
  NPT3 string char -> fromStringChar string char

nptR :: NPTFn r
nptR = gcaseR @NoParamType

nptL :: NoParamType -> r -> (Int -> r) -> (String -> Char -> r) -> r
nptL = gcaseL @NoParamType

specNPT ::
  forall r.
  ( Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  NPTFn r ->
  H.Spec
specNPT name f =
  specG @(FunArgs r)
    ("manual", mkFn manual)
    (name, mkFn f)

mkFn ::
  NPTFn r ->
  NPTFun r
mkFn f r f1 f2 = f r (applyFun f1) (applyFun <$> applyFun f2)

nptL_ :: NPTFn r
nptL_ r fromInt fromStringChar npt = nptL npt r fromInt fromStringChar

spec :: H.Spec
spec = do
  H.describe "()" $ do
    specNPT @() "nptR" nptR
    specNPT @() "nptL" nptL_
  H.describe "Char" $ do
    specNPT @Char "nptR" nptR
    specNPT @Char "nptL" nptL_
  H.describe "String" $ do
    specNPT @String "nptR" nptR
    specNPT @String "nptL" nptL_
  H.describe "[Maybe (Int, String)]" $ do
    specNPT @[Maybe (Int, String)] "nptR" nptR
    specNPT @[Maybe (Int, String)] "nptL" nptL_
