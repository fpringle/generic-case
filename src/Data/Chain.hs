module Data.Chain
  ( -- * Type functions
    Chain
  , toChain
  , fromChain
  , Chains
  , toChains
  , fromChains
  , ChainsL
  , toChainsL
  , fromChainsL
  , ChainsR
  , toChainsR
  , fromChainsR

    -- * Concrete SOP types
  , ChainF (..)
  , applyChain
  , applyNSChain
  , chainFn
  )
where

import Data.SOP.NP
import Data.SOP.NS
import Generics.SOP

{- | Isomorphic to @ChainF r xs@, as witnessed by 'fromChain' and 'toChain'

@
Chain '[x, y, z] r
  ~ (x -> y -> z -> r)
@
-}
type family Chain xs r where
  Chain '[] r = r
  Chain (x ': xs) r = x -> Chain xs r

{- | Convert from type family 'Chain' to concrete type 'ChainF'.

Inverse of 'toChain'
-}
fromChain :: forall xs r. (SListI xs) => Chain xs r -> ChainF r xs
fromChain sc = case sList @xs of
  SNil -> ChainF (const sc)
  SCons ->
    ChainF $ \(I x :* xs) ->
      let ChainF f = fromChain $ sc x
      in  f xs

{- | Convert from concrete type 'ChainF' to type family 'Chain'.

e.g.

@
chainF :: ChainF String '[Int, Maybe Char]
chainF = ChainF $ \(I n :* I mChar :* Nil) -> show n <> " " <> show mChar

singleChain :: Int -> Maybe Char -> String
singleChain = toChain chainF
@
-}
toChain :: forall xs r. (SListI xs) => ChainF r xs -> Chain xs r
toChain (ChainF f) = case sList @xs of
  SNil -> f Nil
  SCons -> \x -> toChain $ ChainF $ \xs -> f (I x :* xs)

{- | Isomorphic to @NP (ChainF final) xss -> ret@, as witnessed by 'toChains' and 'fromChains'.

Chains '[ '[x,y], '[z], '[]] ret final
  ~ Chain '[x,y] final -> Chain '[z] final -> Chain '[] final -> ret
  ~ (x -> y -> final)  -> (z -> final)     -> final           -> ret
-}
type family Chains xss ret final where
  Chains '[] ret final = ret
  Chains (xs ': xss) ret final = Chain xs final -> Chains xss ret final

{- | Convert from a product of concrete types 'ChainF' to type family 'Chains'.

e.g.

@
maybeF :: NP (ChainF Int) '[ '[], '[Char]] -> Maybe Char -> Int
maybeF (ChainF n :* _) Nothing = n Nil
maybeF (_ :* ChainF f :* Nil) (Just c) = f (I c :* Nil)

maybeR :: Int -> (Char -> Int) -> Maybe Char -> Int
maybeR = toChains maybeF

maybeL :: Maybe Char -> Int -> (Char -> Int) -> Int
maybeL mc = toChains (flip maybeF mc)
@
-}
toChains ::
  forall xss ret final.
  (All SListI xss) =>
  (NP (ChainF final) xss -> ret) ->
  Chains xss ret final
toChains f = case sList @xss of
  SNil -> f Nil
  SCons -> \sc -> toChains $ \xs -> f (fromChain sc :* xs)

fromChains ::
  forall xss ret final.
  (All SListI xss) =>
  Chains xss ret final ->
  NP (ChainF final) xss ->
  ret
fromChains r = \case
  Nil -> r
  sc :* cs -> fromChains (r $ toChain sc) cs

{- | Isomorphic to @NP (ChainF r) xss -> a -> r@, as witnessed by 'toChainsR' and 'fromChainsR'.

@
ChainsR '[ '[x,y], '[z], '[]] a r
  ~ Chain '[x,y] r -> Chain '[z] r -> Chain '[] r -> a -> r
  ~ (x -> y -> r)  -> (z -> r)     -> r           -> a -> r
@
-}
type ChainsR xss a r = Chains xss (a -> r) r

toChainsR :: forall xss a r. (All SListI xss) => (NP (ChainF r) xss -> a -> r) -> ChainsR xss a r
toChainsR = toChains

fromChainsR :: forall xss a r. (All SListI xss) => ChainsR xss a r -> (NP (ChainF r) xss -> a -> r)
fromChainsR = fromChains

{- | Isomorphic to @NP (ChainF r) xss -> r@, as witnessed by 'toChainsL' and 'fromChainsL'.

@
ChainsL '[ '[x,y], '[z], '[]] r
  ~ Chain '[x,y] r -> Chain '[z] r -> Chain '[] r -> r
  ~ (x -> y -> r)  -> (z -> r)     -> r           -> r
@
-}
type ChainsL xss r = Chains xss r r

toChainsL :: forall xss r. (All SListI xss) => (NP (ChainF r) xss -> r) -> ChainsL xss r
toChainsL = toChains

fromChainsL :: forall xss r. (All SListI xss) => ChainsL xss r -> (NP (ChainF r) xss -> r)
fromChainsL = fromChains

newtype ChainF r xs = ChainF (NP I xs -> r)

applyChain :: ChainF r xs -> NP I xs -> r
applyChain (ChainF f) = f

chainFn :: ChainF r xs -> (NP I -.-> K r) xs
chainFn = fn . (K .) . applyChain

applyNSChain :: forall r xss. (SListI xss) => NP (ChainF r) xss -> SOP I xss -> r
applyNSChain chains (SOP ns) = collapse_NS $ ap_NS fns ns
  where
    fns = liftA_NP chainFn chains
