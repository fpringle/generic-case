{- | Generic case analysis using [generics-sop](https://hackage.haskell.org/package/generics-sop).

"Case analysis" functions are those which take one function for each constructor of a sum type,
examine a value of that type, and call the relevant function depending on which constructor was
used to build that type. Examples include 'maybe', 'either' and 'Data.Bool.bool'.

It's often useful to define similar functions on user-defined sum types, which is boring at best
and error-prone at worst. This module gives us these functions for any type which
implements 'Generic'.

For any single-constructor types, such as tuples, this gives us generic uncurrying without
any extra effort - see 'tupleR', 'tuple3R'.

== Example

Let's use @These@ from
[these](https://hackage.haskell.org/package/these) as an example.
First we need an instance of 'Generic', which we can derive.

@
{\-# LANGUAGE DeriveGeneric #-\}
import qualified GHC.Generics as G
import Generics.SOP (Generic)

data These a b
  = This a
  | That b
  | These a b
  deriving (Show, Eq, G.Generic)

instance Generic (These a b)      -- we could also do this using DeriveAnyClass
@

We're going to re-implement the case analysis function
[these](https://hackage.haskell.org/package/these-1.2.1/docs/Data-These.html#v:these),
using 'gcaseR'. Our type has 3 constructors, so our function will have 4 arguments:
one function for each constructor, and one for the @These@ we're analysing.
The function is polymorphic in the result type.

@
these ::
  forall a b c.
  _ -> _ -> _ ->
  These a b -> c
@

What are the types of those 3 functions? For each constructor, we make a function type taking
one of each of the argument types, and returning our polymorphic result type @c@:

@
these ::
  forall a b c.
  (a -> c) ->       -- for This
  (b -> c) ->       -- for That
  (a -> b -> c) ->  -- for These
  These a b -> c
@

Finally, we add the implementation, which is just 'gcaseR':

@
these ::
  forall a b c.
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  These a b -> c
these = gcaseR @(These a b)
@

Note that we need the @TypeApplications@ extension here. If you're really against this extension,
see 'gcaseR_'.

For a version that takes the datatype before the functions, see 'gcaseL'.
-}
module Generics.Case
  ( -- * Generic case analysis
    AnalysisR
  , gcaseR
  , gcaseR_
  , AnalysisL
  , gcaseL

    -- * Examples

    -- ** Maybe
  , maybeR
  , maybeL

    -- ** Either
  , eitherR
  , eitherL

    -- ** Bool
  , boolR
  , boolL

    -- ** Tuples
  , tupleR
  , tuple3R
  , tupleL

    -- ** Lists
  , listR
  , listL

    -- ** Non-empty lists
  , nonEmptyR
  , nonEmptyL
  )
where

import Data.List.NonEmpty (NonEmpty)
import Generics.Chain
import Generics.SOP

{- | The type of an analysis function on a generic type, in which the type comes after the functions.

You shouldn't ever need to create a function of this type; use 'gcaseR' or 'gcaseR_'.

You can exapand the type in a repl:

@
ghci> :k! AnalysisR (Maybe a) r
AnalysisR (Maybe a) r :: *
= r -> (a -> r) -> Maybe a -> r
@
-}
type AnalysisR a r = ChainsR (Code a) a r

{- | Same as 'AnalysisR', but the type being anlaysed comes before the functions.

You shouldn't ever need to create a function of this type; use 'gcaseL'.

@
ghci> :k! AnalysisL (Maybe a) r
AnalysisL (Maybe a) r :: *
= Maybe a -> r -> (a -> r) -> r
@
-}
type AnalysisL a r = a -> ChainsL (Code a) r

{- | Generic case analysis, with the same shape as 'maybe' or 'either' (functions before dataype).

See the module header for a detailed explanation.
-}
gcaseR ::
  forall a r.
  (Generic a) =>
  AnalysisR a r
gcaseR = toChains @(Code a) @(a -> r) f
  where
    f c a = applyNSChain c (from a)

{- | Morally the same as 'gcaseR', but takes a 'Proxy' to avoid @TypeApplications@.

Following our @These@ example:

@
these_ ::
  forall a b c.
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  These a b -> c
these_ = gcaseR_ (Proxy :: Proxy (These a b))
@
-}
gcaseR_ ::
  forall a r.
  (Generic a) =>
  Proxy a ->
  AnalysisR a r
gcaseR_ _ = gcaseR @a @r

{- | Simliar to 'gcaseR', except the type being analysed comes before the functions, instead of
after.

Unlike @gcaseR@, this shouldn't need @TypeApplications@.

Following our @These@ example:

@
theseL ::
  forall a b c.
  These a b ->
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  c
theseL = gcaseL
@
-}
gcaseL ::
  forall a r.
  (Generic a) =>
  AnalysisL a r
gcaseL a = toChains @(Code a) @r f
  where
    f c = applyNSChain c (from a)

------------------------------------------------------------
-- Examples

{- | 'maybe', implemented using 'gcaseR_'.

The implementation is just:

@
maybeR = gcaseR_ (Proxy :: Proxy (Maybe a))
@
-}
maybeR :: forall a r. r -> (a -> r) -> Maybe a -> r
maybeR = gcaseR_ (Proxy :: Proxy (Maybe a))

{- | Same as 'maybeR', except the 'Maybe' comes before the case functions.

The implementation is just:

@
maybeL = gcaseL @(Maybe a)
@
-}
maybeL :: forall a r. Maybe a -> r -> (a -> r) -> r
maybeL = gcaseL

{- | 'either', implemented using 'gcaseR'.

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
eitherL = gcaseL
@
-}
eitherL :: forall a b r. Either a b -> (a -> r) -> (b -> r) -> r
eitherL = gcaseL

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
boolL = gcaseL
@
-}
boolL :: forall a. Bool -> a -> a -> a
boolL = gcaseL

{- | Case analysis on a list. Same as
[list](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:list)
from @extra@.
-}
listR :: forall a r. r -> (a -> [a] -> r) -> [a] -> r
listR = gcaseR @[a]

-- | Same as 'listR', except the list comes before the case functions.
listL :: forall a r. [a] -> r -> (a -> [a] -> r) -> r
listL = gcaseL

-- | Case analysis on a tuple. Interestingly, this is the same as 'uncurry'.
tupleR :: forall a b r. (a -> b -> r) -> (a, b) -> r
tupleR = gcaseR @(a, b)

{- | Case analysis on a 3-tuple. Same as
[uncurry3](https://hackage.haskell.org/package/extra/docs/Data-Tuple-Extra.html#v:uncurry3)
from @extra@.
-}
tuple3R :: forall a b c r. (a -> b -> c -> r) -> (a, b, c) -> r
tuple3R = gcaseR @(a, b, c)

-- | Same as 'tupleR', except the tuple comes before the case function.
tupleL :: forall a b r. (a, b) -> (a -> b -> r) -> r
tupleL = gcaseL

-- | Case analysis on a non-empty list.
nonEmptyR :: forall a r. (a -> [a] -> r) -> NonEmpty a -> r
nonEmptyR = gcaseR @(NonEmpty a)

-- | Same as 'nonEmptyR', except the non-empty list comes before the case function.
nonEmptyL :: forall a r. NonEmpty a -> (a -> [a] -> r) -> r
nonEmptyL = gcaseL
