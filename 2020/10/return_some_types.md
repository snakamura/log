# Function returning some types

In the previous two posts ([part 1](https://snak.tumblr.com/post/631687468085886976/functions-taking-some-types-part-1) and [part 2](https://snak.tumblr.com/post/631854525488201729/functions-taking-some-types-part-2)), we discussed how we can define functions taking some types. Now, we're going to think about functions returning some types.

First, let's define `S` and `X`. They're identical to `S` and `X` in the previous posts.

```
{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds,
             DataKinds,
             GADTs,
             EmptyCase,
             InstanceSigs,
             OverloadedStrings,
             PolyKinds,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind
    ( Constraint
    , Type
    )
import Data.Singletons.Prelude (Elem)
import Data.Singletons.Sigma
    ( Sigma((:&:))
    , projSigma2
    )
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
  |]

data X (s :: S) where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)
```

Now we're going to define functions that returns `X 'S2` or `X 'S3`. Obviously, the simplest approach would be using `Either`.

```
f1 :: Bool -> X 'S1 -> Either (X 'S2) (X 'S3)
f1 True x = Left $ X2 2
f1 False x = Right $ X3 "3"

c1 :: Text
c1 = case f1 True X1 of
         Left (X2 n) -> T.pack $ show n
         Right (X3 t) -> t
```

The problem of using `Either` is that we need to nest `Either`s or define a new sum type when a function returns more than two types.

Another option would be returning `Sigma S (TyCon X)`. [`Sigma`](http://hackage.haskell.org/package/singletons-2.6/docs/Data-Singletons-Sigma.html#t:Sigma) is an existential type indexed by a singleton. You can think it as a generic version of `SomeX` in [the previous post](https://snak.tumblr.com/post/631854525488201729/functions-taking-some-types-part-2). Just like you used `SomeX SS2 (X2 2)` to express `X2 2` in an existential type, you can use `SS2 :&: X2 2`.

Using `Sigma S (TyCon S)`, you can write `f2` like this.

```
f2 :: Bool -> X 'S1 -> Sigma S (TyCon X)
f2 True x = SS2 :&: X2 2
f2 False x = SS3 :&: X3 "3"

c2 :: Text
c2 = projSigma2 p $ f2 True X1
  where
    p :: X s -> Text
    p X1 = undefined
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
    p (X4 _) = undefined
```

As you can see, the problem is that this doesn't express the idea that `f2` only returns `X 'S2` or `X 'S3`, but never returns `X 'S1` nor `X 'S4`. So you need to pattern match on all constructors of `X` when you call it.

How about wrapping them by another data type indexed by the same `S` to indicate that the function only return `X 'S2` or `X 'S3`?

```
data F3 :: S -> Type where
    F32 :: X 'S2 -> F3 'S2
    F33 :: X 'S3 -> F3 'S3

data F3Sym0 :: S ~> Type
type instance Apply F3Sym0 x = F3 x

f3 :: Bool -> X 'S1 -> Sigma S F3Sym0
f3 True x = SS2 :&: F32 (X2 2)
f3 False x = SS3 :&: F33 (X3 "3")

c3 :: Text
c3 = projSigma2 p $ f3 True X1
  where
    p :: F3 s -> Text
    p (F32 (X2 n)) = T.pack $ show n
    p (F33 (X3 t)) = t
```

This works, but we have to update `F3` and we always need to unwrap `F3` explicitly when you call `f3`. I don't think this is better than defining a sum type and directly returning it.

Okay, then what we need is an existential type like `Sigma`, but has a constraint on its index type. Let's define such type `SigmaP`.

```
data SigmaP (s :: Type) (p :: s ~> Constraint) (t :: s ~> Type) where
    (:&?:) :: (p @@ fst) => Sing (fst :: s) -> t @@ fst -> SigmaP s p t

projSigmaP2 :: forall s p t r. (forall (fst :: s). p @@ fst => (t @@ fst) -> r) -> SigmaP s p t -> r
projSigmaP2 f ((_ :: Sing (fst :: s)) :&?: b) = f @fst b
```

Now, you can create a type function `F4` to generate a constraint and use it with `SigmaP`.

```
type family F4 (x :: S) :: Constraint where
    F4 S2 = ()
    F4 S3 = ()
    F4 _ = ('True ~ 'False)

data F4Sym0 :: S ~> Constraint
type instance Apply F4Sym0 x = F4 x

f4 :: Bool -> X 'S1 -> SigmaP S F4Sym0 (TyCon X)
f4 True x = SS2 :&?: X2 2
f4 False x = SS3 :&?: X3 "3"

c4 :: Text
c4 = projSigmaP2 p $ f4 True X1
  where
    p :: F4 s => X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
```

It's got better, but we still need to define `F4`. How can I avoid this? One of the ideas is generating a constraint from a list of types.

```
type family OneOf l t :: Constraint where
    OneOf l t = If (Elem t l) (() :: Constraint) ('True ~ 'False)

data OneOfSym0 :: l ~> t ~> Constraint
type instance Apply OneOfSym0 x = OneOfSym1 x
data OneOfSym1 :: l -> t ~> Constraint
type instance Apply (OneOfSym1 l) t = OneOf l t
type OneOfSym2 l t = OneOf l t
```

`OneOf` generates a valid constraint only when its second argument is included in the first argument. Note that we need to generate `OneOfSym0`, `OneOfSym1` and `OneOfSym2` so that we can pass them to `SigmaP` as a defunctionalized symbol. You can manually define them like this, or you can let `singletons` generate them.

```
genDefunSymbols [''OneOf]
```

Anyway, armed with `SigmaP` and `OneOf`, you can write `f5`.

```
f5 :: Bool -> X 'S1 -> SigmaP S (OneOfSym1 '[ 'S2, 'S3 ]) (TyCon X)
f5 True x = SS2 :&?: X2 2
f5 False x = SS3 :&?: X3 "3"

c5 :: Text
c5 = projSigmaP2 p $ f5 True X1
  where
    p :: OneOf '[ 'S2, 'S3 ] s => X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
```

If you think combining `SigmaP` and `OneOf` is too much, you can write `SigmaL` which directly generates a constraint from a list of types.

```
data SigmaL (l :: [s :: Type]) (t :: s ~> Type) where
    (:&!:) :: OneOf l fst => Sing (fst :: s) -> t @@ fst -> SigmaL l t

projSigmaL2 :: forall s (l :: [s]) t r. (forall (fst :: s). OneOf l fst => (t @@ fst) -> r) -> SigmaL l t -> r
projSigmaL2 f ((_ :: Sing (fst :: s)) :&!: b) = f @fst b

f6 :: Bool -> X 'S1 -> SigmaL '[ 'S2, 'S3 ] (TyCon X)
f6 True x = SS2 :&!: X2 2
f6 False x = SS3 :&!: X3 "3"

c6 :: Text
c6 = projSigmaL2 p $ f6 True X1
  where
    p :: OneOf '[ 'S2, 'S3 ] s => X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
```
