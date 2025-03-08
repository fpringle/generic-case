{- | Generic case analysis using [generics-sop](https://hackage.haskell.org/package/generics-sop).

"Case analysis" functions are those which take one function for each constructor of a sum type,
examine a value of that type, and call the relevant function depending on which constructor was
used to build that type. Examples include 'maybe', 'either' and 'Data.Bool.bool'.

It's often useful to define similar functions on user-defined sum types, which is boring at best
and error-prone at worst. This module gives us these functions for any type which
implements 'Generic'.

For any single-constructor types, such as tuples, this gives us generic uncurrying without
any extra effort - see 'tupleL', 'tuple3L'.

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
using 'gcase'. Our type has 3 constructors, so our function will have 4 arguments:
one for the @These@ we're analysing, and one function for each constructor.
The function is polymorphic in the result type.

@
these ::
  forall a b c.
  These a b ->
  _ -> _ -> _ ->
  c
@

What are the types of those 3 functions? For each constructor, we make a function type taking
one of each of the argument types, and returning our polymorphic result type @c@:

@
these ::
  forall a b c.
  These a b ->
  (a -> c) ->       -- for This
  (b -> c) ->       -- for That
  (a -> b -> c) ->  -- for These
  c
@

Finally, we add the implementation, which is just 'gcase':

@
these ::
  forall a b c.
  These a b ->
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  c
these = gcase
@

Note that we could have written the entire thing more succintly using 'Analysis':

@
these ::
  forall a b c.
  Analysis (These a b) c
these = gcase
@
-}
module Generics.Case
  ( -- * Generic case analysis
    Analysis
  , gcase

    -- * Examples

    -- ** Maybe
  , maybeL

    -- ** Either
  , eitherL

    -- ** Bool
  , boolL

    -- ** Tuples
  , tupleL
  , tuple3L

    -- ** Lists
  , listL

    -- ** Non-empty lists
  , nonEmptyL
  )
where

import Data.List.NonEmpty (NonEmpty)
import Generics.Chain
import Generics.SOP

{- | The type of an analysis function on a generic type, in which the type comes before the functions.

You shouldn't ever need to create a function of this type manually; use 'gcase'.

You can exapand the type in a repl:

@
ghci> :k! Analysis (Maybe a) r
Analysis (Maybe a) r :: *
= Maybe a -> r -> (a -> r) -> r
@
-}
type Analysis a r = a -> Chains (Code a) r

{- | Generic case analysis. Similar to 'maybe' or 'either', except the type being analysed comes
before the functions, instead of after.

See the module header for a detailed explanation.
-}
gcase ::
  forall a r.
  (Generic a) =>
  Analysis a r
gcase = applyChains @(Code a) @r . unSOP . from

------------------------------------------------------------
-- Examples

{- | Same as 'maybe', except the 'Maybe' comes before the case functions.

Equivalent type signature:

@
maybeL :: forall a r. Analysis (Maybe a) r
@

The implementation is just:

@
maybeL = gcase @(Maybe a)
@
-}
maybeL :: forall a r. Maybe a -> r -> (a -> r) -> r
maybeL = gcase

{- | Same as 'either', except the 'Either' comes before the case functions.

Equivalent type signature:

@
eitherL :: forall a b r. 'Analysis' (Either a b) r
@

The implementation is just:

@
eitherL = gcase
@
-}
eitherL :: forall a b r. Either a b -> (a -> r) -> (b -> r) -> r
eitherL = gcase

{- | Same as 'Data.Bool.bool', except the 'Bool' comes before the case functions.

Equivalent type signature:

@
boolL :: forall r. 'Analysis' Bool r
@

The implementation is just:

@
boolL = gcase
@
-}
boolL :: forall r. Bool -> r -> r -> r
boolL = gcase

{- | Case analysis on a list. Same as
[list](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:list)
from @extra@, except the list comes before the case functions.

Equivalent type signature:

@
listL :: forall a r. 'Analysis' [a] r
@
-}
listL :: forall a r. [a] -> r -> (a -> [a] -> r) -> r
listL = gcase

{- | Case analysis on a tuple. Same as 'uncurry', except the tuple comes before the case function.

Equivalent type signature:

@
tupleL :: forall a b r. 'Analysis' (a, b) r
@
-}
tupleL :: forall a b r. (a, b) -> (a -> b -> r) -> r
tupleL = gcase

{- | Case analysis on a 3-tuple. Same as
[uncurry3](https://hackage.haskell.org/package/extra/docs/Data-Tuple-Extra.html#v:uncurry3)
from @extra@, except the tuple comes before the case function.

Equivalent type signature:

@
tupleL :: forall a b c r. 'Analysis' (a, b, c) r
@
-}
tuple3L :: forall a b c r. (a, b, c) -> (a -> b -> c -> r) -> r
tuple3L = gcase

{- | Case analysis on a non-empty list.

Equivalent type signature:

@
nonEmptyL :: forall a r. 'Analysis' (NonEmpty a) r
@
-}
nonEmptyL :: forall a r. NonEmpty a -> (a -> [a] -> r) -> r
nonEmptyL = gcase
