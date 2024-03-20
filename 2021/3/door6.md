# Open the door with types, part 6

In the previous posts, I used `Either` for a return type of `unlock`. It returns `Right` when it successfully unlocked the door, and `Left` otherwise. This works well as long as it only returns one of the two types, but it'll get complicated when it needs to return one of the five types, for example.

You can use [`Sigma`](https://hackage.haskell.org/package/singletons-3.0/docs/Data-Singletons-Sigma.html#t:Sigma) when you want to make a function return one of multiple types. But in our situation, we need to return one of two types out of three types. We have `Door 'Opened`, `Door 'Closed` and `Door 'Locked`, but `unlock` only returns `Door 'Closed` or `Door 'Locked`.

To limit the types a function can return, you can use `SigmaP` which I explained in [Function returning some types](../../2020/10/return_some_types.html). You'll pass a custom constraint to `SigmaP` to limit types.

Let's review `SigmaP` and a helper type function `OneOf`.

```
{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds,
             DataKinds,
             GADTs,
             KindSignatures,
             PolyKinds,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneKindSignatures,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

module Sigma
    ( SigmaP((:&?:))
    , projSigmaP2
    , OneOf
    , OneOfSym0
    , OneOfSym1
    ) where

import Data.Kind (Constraint, Type)
import Data.Singletons.TH

data SigmaP (s :: Type) (p :: s ~> Constraint) (t :: s ~> Type) where
    (:&?:) :: (p @@ fst) => Sing (fst :: s) -> t @@ fst -> SigmaP s p t

projSigmaP2 :: forall s p t r. (forall (fst :: s). p @@ fst => (t @@ fst) -> r) -> SigmaP s p t -> r
projSigmaP2 f ((_ :: Sing (fst :: s)) :&?: b) = f @fst b


type family OneOf l t :: Constraint where
    OneOf l t = If (Elem t l) (() :: Constraint) ('True ~ 'False)

genDefunSymbols [''OneOf]
```

Using them, you can define `unlock` like this.

```
unlock :: Text -> Door 'Locked -> SigmaP State (OneOfSym1 '[ 'Closed, 'Locked ]) (TyCon Door)
unlock key lockedDoor@(LockedDoor name lockedKey)
    | key == lockedKey = SClosed :&?: ClosedDoor name
    | otherwise = SLocked :&?: lockedDoor
```

When you match the first part of `SigmaP`, you can convince your compiler that the second part is a specific type. For example, you can write `forceOpen` like this.

```
forceOpen :: forall state. SingI state => Text -> Door state -> Maybe (Door 'Opened)
forceOpen key door =
    case sing @state of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     SClosed :&?: closedDoor -> Just $ open closedDoor
                     SLocked :&?: _ -> Nothing
```

As you can see, when the first part is `SClosed`, the second part must be `Door 'Closed`. When the first part is `SLocked`, the second part must be `Door 'Locked`.

It's very similar to using `Either`. When it returns `Left`, it must contain `Door 'Closed`, and it must contain `Door 'Locked` if it returns `Right`. But `Sigma` (and `SigmaP`) works even when the function returns one of more than two types.
