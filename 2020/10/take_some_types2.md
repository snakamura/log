# Functions taking some types, part 2

In [the previous post](./take_some_types1.html), we looked at how we can define functions taking some types with a phantom type. However, all of those functions need to know an actual type at the compile time. How can define functions that take some types which will be known at runtime?

First, let's pick a function from the previous post.

```
{-# LANGUAGE DataKinds,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             LambdaCase,
             PolyKinds,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind (Type)
import Data.Singletons.Decide (Decision(Proved, Disproved))
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
  |]

data X :: S -> Type where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)


data F :: S -> Type where
    F2 :: F 'S2
    F3 :: F 'S3

f :: F s -> X s -> Text
f F2 (X2 n) = T.pack $ show n
f F3 (X3 t) = t
```

This function takes a witness saying `s` is one of `'S2` or `'S3` (`F s`) and actual value (`X s`), and was named `f3` in the previous post. We'll see how we can call this function from functions that don't know the parameter type at the compile time.

Now, let's define `SomeX`. This type has `X s` in it, but indexes it by `Sing s`. This type represents `X` whose actual type is unknown until runtime.

```
data SomeX where
    SomeX :: Sing s -> X s -> SomeX
```

The first example is pattern-matching `Sing s` in `Some X`. Since `s` in `Sing s` and `X s` are unified, the compiler can know which type `X s` is.

```
f1 :: SomeX -> Maybe Text
f1 (SomeX SS1 _) = Nothing
f1 (SomeX SS2 x) = Just $ f F2 x
f1 (SomeX SS3 x) = Just $ f F3 x
f1 (SomeX SS4 _) = Nothing

c1 :: Maybe Text
c1 = f1 $ SomeX SS2 $ X2 2
```

This is pretty straightforward, but there is one problem. Imagine you've updated `F` and `f` to accept `X 'S4` like this, but you forgot updating `f1`.

```
data F :: S -> Type where
    F2 :: F 'S2
    F3 :: F 'S3
    F4 :: F 'S4

f :: F s -> X s -> Text
f F2 (X2 n) = T.pack $ show n
f F3 (X3 t) = t
f F4 (X4 f) = T.pack $ show f
```

Even though `f` accepts `X 'S4`, `f1` still returns `Nothing`. And the compiler never tells you about it. What can we do?

It's time to take advantage of `Decision` from `Data.Singletons.Decicde`. Let's define `isF` which tells your compiler which `s` it can use with `F`.

```
isF :: Sing s -> Decision (F s)
isF SS1 = Disproved $ \case {}
isF SS2 = Proved F2
isF SS3 = Proved F3
isF SS4 = Disproved $ \case {}
```

The `case`s in `Disproved` for `SS1` and `SS4` are empty because there are no patterns matching it. In these `case`s, the compiler knows that it's pattern-matching against `F 'S1` and `F 'S4`, but there are no such values in `F`.

You can define `f2` using this `isF` this way.

```
f2 :: SomeX -> Maybe Text
f2 (SomeX s x) | Proved p <- isF s = Just $ f p x
               | otherwise = Nothing

c2 :: Maybe Text
c2 = f2 $ SomeX SS2 $ X2 2
```

So what happens when you change `F` and `f` to accept `X 'S4`? GHC tells you the pattern is non-exhaustive.

```
paramdynamic.hs:65:23: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: F4
   |
65 | isF SS4 = Disproved $ \case {}
   |                       ^^^^^^^^
```

This is because the compiler knows there is a value of `F 'S4` now. Writing `f2` is bit complicated than `f1`, but the compiler will give you some sort of safety if you do so.
