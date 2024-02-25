# Combining two functions in a type-safe way

Imagine that you have types `A` and `B`, a function returns `A` or `B` depending on a `Bool` value, and a function returning a function from `A` to `String` or `B` to `String`.

```
data A = A
data B = B

value :: Bool -> Either A B
value True = Left A
value False = Right B

func :: Bool -> Either (A -> String) (B -> String)
func True = Left $ \A -> "A"
func False = Right $ \B -> "B"
```

Now you have a function that takes a `Bool` and returns a `String`. This function calls `value` and pass its return value to `func`.

```
test :: Bool -> String
test useA = case (value useA, func useA) of
  (Left a, Left fa) -> fa a
  (Right b, Right fb) -> fb b
  _ -> error "Never happens"
```

The `case` isn't exhaustive even though the error case won't happen.

Can you make this type-safe?

The first thing you need to do is making `value` and `func` not take a `Bool` value, but take one of two types so that it can return a value of a different type based on it. Since the return type cannot depend on a value (`True` or `False`), you need to pass different types.

You can use lifted types `'True` and `'False` for it, but you cannot use them directly because their kind is `Bool`, not `Type`.

So now, we'll create a type named `SBool` that takes `'True` or `'False`. Since its kind is `Type` (when you pass `Bool`), you can use it as a type.

```
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Type)

data A = A
data B = B

type SBool :: Bool -> Type
data SBool b where
  STrue :: SBool 'True
  SFalse :: SBool 'False
```

As you can see, passing `STrue` means `b` is `'True`, and passing `SFalse` means `b` is `'False`. So we can now pass either of `'True` or `'False` wrapped in `SBool`.

Next, we need a type function that returns `A` if its parameter is `'True`, and `B` if `'False`.

```
type R :: Bool -> Type
type family R b where
  R 'True = A
  R 'False = B
```

With them, you can now write `value` and `func`. You'll pass `b` in `SBool` to `R` to get a desired type. For example, when you pass `STrue` whose type is `SBool 'True`, a return type of `value` becomes `R 'True` which is `A`.

```
value :: SBool b -> R b
value STrue = A
value SFalse = B

func :: SBool b -> R b -> String
func STrue A = "A"
func SFalse B = "B"
```

Before implementing `test`, we need one more type; an existential type to wrap `SBool`. Since `SBool 'True` and `SBool 'False` are different types, you cannot mix them without it.

```
data SomeBool where
  SomeBool :: SBool b -> SomeBool
```

In `test`, we'll convert `Bool` to `SBool`, then pass it to `value` and `func`. The compiler now knows `value` returns `A` and `func` returns `A -> String`, or `B` and `B -> String`, you can pass a value returned from `value` to `func` in a type-safe manner.

```
test :: Bool -> String
test useA =
  let sc = if useA then SomeBool STrue else SomeBool SFalse
   in case sc of
        SomeBool sb -> func sb (value sb)
```
