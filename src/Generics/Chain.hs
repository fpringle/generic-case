{- | Uniform representation + handling of n-ary functions.

This module gives us types and functions (both value- and type-level) to work
with n-ary functions.
The following are all function types, yet have very different shapes:

@
f1 :: Int -> Int
f1 = undefined
f2 :: a -> (b, a) -> c
f2 = undefined
f3 :: a -> (a -> a) -> (a -> a -> a) -> a
f3 = undefined
@

However there are 2 ways we can "unify" these into a common structure.
Both ways involve diving the function type into arguments (e.g. @Int ->@,
@a -> (b, a) ->@ and @a -> (a -> a) -> (a -> a -> a) ->@), and result types
(e.g. @Int@, @c@ and @a@).

The first way is to see these functions as right-associative folds.
Imagine a type-level function directly corresponding to 'foldr':

@
type family Foldr cons xs nil where
  Foldr cons '[] nil = nil
  Foldr cons (x ': xs) nil = cons x (Foldr cons xs nil)
@

Then using the function arrow @(->)@ for @cons@, the result type of our function
for @nil@ and the list of arguments for @xs@:

@
f1_ :: Foldr (->) '[Int] Int
f1_ = f1
f2_ :: Foldr (->) '[a, (b, a)] c
f2_ = f2
f3_ :: Foldr (->) '[a, a -> a, a -> a -> a] a
f3_ = f3
@

The 'Chain' family does exactly that. Since GHC can unify these types, we
can use 'Chain' in our types signatures in "Generics.Case" and the user doesn't
have to think about SOP, generics etc.

However, it's hard to manipulate a 'Chain' in a generic way. We want a principled
way to manipulate any function type, regardless of how many arguments it has.
That's where [sop-core](https://hackage.haskell.org/package/sop-core) comes in.

The second way to unify all function types is using uncurrying: rather than a series of
function arrows, we see a function as mapping a tuple (or a heterogeneous list) to a result type:

@
f1_tuple :: (Int) -> Int
f2_tuple :: (a, (b, a)) -> c
f3_tuple :: (a, a -> a, a -> a -> a) -> a
@

Or, in SOP terminology:

@
f1_NP_ :: NP '[Int] -> Int
f2_NP_ :: NP '[a, (b, a)] -> c
f3_NP_ :: NP '[a, a -> a, a -> a -> a] -> a
@

Since these types are equivalent, once we convert to the 'NP' representation we can then manipulate
them using all the usual SOP machinery, and then convert back.
The 'ChainF' type does exactly that, and the 'toChain' and 'fromChain' functions allow us to
convert between the two representations on the value level:

@
f1_NP :: ChainF Int '[Int]
f1_NP = fromChain f1
f2_NP :: ChainF c '[a, (b, a)]
f2_NP = fromChain f2
f3_NP :: ChainF a '[a, a -> a, a -> a -> a]
f3_NP = fromChain f3
@

Note that the argument list now comes after the result type: this is so that we can use 'ChainF'
with 'NP' etc.

Unlike 'Chain', 'ChainF' is a concrete type using SOP stuff. Ideally we don't want to expose
it to the user and force them to supply functions that take 'NP's as arguments.

'Chains' and @'NP' ('ChainF' r)@ iterate on these concepts: 'Chains' is a type-level family represent
a function of functions, and @'NP' ('ChainF' r)@ is the SOP equivalent. We can convert between them
using 'toChains' and 'fromChains'. This lets us represent "case analysis" functions like
'maybe' and 'either' nicely (see "Generics.Case"):

@
maybe' :: forall a r. 'ChainsR' '[ '[], '[a]] (Maybe a) r
maybe' = 'maybe'

either' :: forall a b r. 'ChainsR' '[ '[a], '[b]] (Either a b) r
either' = 'either'
@

'ChainsL' and 'ChainsR' are just variants of 'Chains' that allow us to decide whether the
type we're analysing comes before or after the analysis functions.
-}
module Generics.Chain
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

import Data.SOP
import Data.SOP.NP
import Data.SOP.NS

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

Inverse of 'toChain'.
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

@
Chains '[ '[x,y], '[z], '[]] ret final
  ~ Chain '[x,y] final -> Chain '[z] final -> Chain '[] final -> ret
  ~ (x -> y -> final)  -> (z -> final)     -> final           -> ret
@
-}
type family Chains xss ret final where
  Chains '[] ret final = ret
  Chains (xs ': xss) ret final = Chain xs final -> Chains xss ret final

{- | Convert from a function of a product of concrete types 'ChainF' to type family 'Chains'.

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

{- | Convert from type family 'Chains' to a function of a product of concrete types 'ChainF'.

Inverse of 'toChains'.
-}
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

-- | Specialisation of 'toChains' to 'ChainsR'.
toChainsR :: forall xss a r. (All SListI xss) => (NP (ChainF r) xss -> a -> r) -> ChainsR xss a r
toChainsR = toChains

-- | Specialisation of 'fromChains' to 'ChainsR'.
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

-- | Specialisation of 'toChains' to 'ChainsL'.
toChainsL :: forall xss r. (All SListI xss) => (NP (ChainF r) xss -> r) -> ChainsL xss r
toChainsL = toChains

-- | Specialisation of 'fromChains' to 'ChainsL'.
fromChainsL :: forall xss r. (All SListI xss) => ChainsL xss r -> (NP (ChainF r) xss -> r)
fromChainsL = fromChains

{- | A concrete type that is equivalent to an n-ary function.

@ChainF r '[x, y, z]@ is isomorphic to @'Chain' '[x, y, z] r@, which simplifies to
@x -> y -> z -> r@. This isomorphism is witnessed by 'toChain' and 'fromChain'
-}
newtype ChainF r xs = ChainF (NP I xs -> r)

-- | Unwrap a 'ChainF'.
applyChain :: ChainF r xs -> NP I xs -> r
applyChain (ChainF f) = f
{-# INLINE applyChain #-}

{- | Convert a 'ChainF' to a '-.->' function which maps a product of @xs@ to a single value @r@
(more accurately @'K' r@).
-}
chainFn :: ChainF r xs -> (NP I -.-> K r) xs
chainFn = fn . (K .) . applyChain
{-# INLINE chainFn #-}

-- | Apply a product of 'ChainF's to a __sum__ of 'NP's.
applyNSChain :: forall r xss. (SListI xss) => NP (ChainF r) xss -> SOP I xss -> r
applyNSChain chains (SOP ns) = collapse_NS $ ap_NS fns ns
  where
    fns = liftA_NP chainFn chains
