module Data.Case
  ( -- * Generic case analysis
    gcaseR
  , gcaseL

    -- * Examples
  , maybeR
  , maybeL
  , eitherR
  , eitherL
  , boolR
  , boolL
  )
where

import Data.Chain
import Generics.SOP

gcaseR ::
  forall a r.
  (Generic a) =>
  ChainsR (Code a) a r
gcaseR = toChains @(Code a) @(a -> r) f
  where
    f c a = applyNSChain c (from a)

gcaseL ::
  forall a r.
  (Generic a) =>
  a ->
  ChainsL (Code a) r
gcaseL a = toChains @(Code a) @r f
  where
    f c = applyNSChain c (from a)

{- | 'Data.Maybe.maybe', implemented using 'gcaseR'.

The implementation is just:

@
maybeR = gcaseR @(Maybe a)
@
-}
maybeR :: forall a r. r -> (a -> r) -> Maybe a -> r
maybeR = gcaseR @(Maybe a)

{- | Same as 'maybeR', except the 'Maybe' comes before the case functions.

The implementation is just:

@
maybeL = gcaseL @(Maybe a)
@
-}
maybeL :: forall a r. Maybe a -> r -> (a -> r) -> r
maybeL = gcaseL @(Maybe a)

{- | 'Data.Either.either', implemented using 'gcaseR'.

The implementation is just:

@
eitherR = gcaseR @(Either a b)
@
-}
eitherR :: forall a b r. (a -> r) -> (b -> r) -> Either a b -> r
eitherR = gcaseR @(Either a b)

{- | Same as 'eitherR', except the 'Either' comes before the case functions.

The implementation is just:

@
eitherL = gcaseL @(Either a b)
@
-}
eitherL :: forall a b r. Either a b -> (a -> r) -> (b -> r) -> r
eitherL = gcaseL @(Either a b)

{- | 'Data.Bool.bool', implemented using 'gcaseR'.

The implementation is just:

@
boolR = gcaseR @Bool
@
-}
boolR :: forall a. a -> a -> Bool -> a
boolR = gcaseR @Bool

{- | Same as 'boolR', except the 'Bool' comes before the case functions.

The implementation is just:

@
boolL = gcaseL @Bool
@
-}
boolL :: forall a. Bool -> a -> a -> a
boolL = gcaseL @Bool
