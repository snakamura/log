# Implementing length-indexed vector using dependent types

In [Implementing Maybe using dependent types](https://snak.tumblr.com/post/634446023985643520/implementing-maybe-using-dependent-types), I implemented `Maybe` using dependent types. The type itself is `Optional` which is indexed by kind `O`. In kind `O`, there are two types `S` and `N`.

So it's basically a type indexed by a kind with two types. Then, how does it look like if we implement a type indexed by a kind with infinite number of types? Let's try it.

First, let's define a kind which have infinite number of types.

```
{-# LANGUAGE DataKinds,
             GADTs,
             RankNTypes,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TypeOperators
#-}

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))

data Nat = Z | S Nat

type Nat0 = 'Z
type Nat1 = 'S 'Z
type Nat2 = 'S Nat1
type Nat3 = 'S Nat2
```

`Nat` kind has infinite number of types such as `'Z`, `'S 'Z`, `'S ('S 'Z)` and so on.

We need a singleton type for it to use it as an index kind.

```
type SNat :: Nat -> Type
data SNat n where
    SZ :: SNat 'Z
    SS :: SNat m -> SNat ('S m)
deriving instance Show (SNat n)
```

Now that we have an index kind, let's define a type indexed by this kind. This time, we'll define a length-indexed vector.

```
type Vec :: Nat -> Type -> Type
data Vec len a where
    VCons :: a -> Vec len a -> Vec ('S len) a
    VNil :: Vec 'Z a
deriving instance Show a => Show (Vec len a)
```

For example, you can use it like this.

```
v :: Vec ('S ('S 'Z')) Int
v = VCons 1 (VCons 2 VNil)
```

We also define an existential type wrapping this type.

```
data SomeVec a = forall len. SomeVec (SNat len) (Vec len a)
deriving instance Show a => Show (SomeVec a)
```

The next thing we'd like to do is to constract it from an ordinal list.

```
vec :: [a] -> SomeVec a
vec [] = SomeVec SZ VNil
vec (x:xs) = case vec xs of
               SomeVec sLen v -> SomeVec (SS sLen) (VCons x v)
```

Now, we can define functions which have restrictions on length of vectors it takes as arguments. For example, we can define a function calculating sum of a vector whose length is 3.

```
sum3 :: Monoid a => Vec Nat3 a -> a
sum3 (VCons a1 (VCons a2 (VCons a3 VNil))) = a1 <> a2 <> a3
```

If you have an ordinal list, you con convert it to `SomeVec` and call `sum3` only if its length is 3.

```
trySum3 :: Monoid a => [a] -> Maybe a
trySum3 xs = case vec xs of
               SomeVec (SS (SS (SS SZ))) v -> Just $ sum3 v
               _ -> Nothing
```

As you can see, if `SNat len` of `SomeVec` is `SS (SS (SS SZ))`, `len` must be `'S ('S ('S 'Z))`, so you can pass it to `sum3`.

Let's take a look at another example. This time, we'll define a `zip` function which only works on two vectors of the same length.

```
vzip :: Vec len a -> Vec len b -> Vec len (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) $ vzip xs ys
```

Let's call this `vzip` with two ordinal lists. To do this, we need a function checking propositional equality of `SNat`. It returns `Just Refl` if both types are the same, and returns `Nothing` otherwise.

```
sameSNat :: SNat n -> SNat m -> Maybe (n :~: m)
sameSNat SZ SZ = Just Refl
sameSNat (SS sNat1) (SS sNat2) = case sameSNat sNat1 sNat2 of
                                   Just Refl -> Just Refl
                                   Nothing -> Nothing
sameSNat _ _ = Nothing
```

Now you can convert ordinal lists to `SomeVec`s and check if their length are equal using `sameSNat`, and call `vzip` if they are the same.

```
tryZip :: [a] -> [b] -> Maybe (SomeVec (a, b))
tryZip xs ys = case (vec xs, vec ys) of
                 (SomeVec sLenX vx, SomeVec sLenY vy) ->
                   case sameSNat sLenX sLenY of
                     Just Refl -> Just (SomeVec sLenX (vzip vx vy))
                     Nothing -> Nothing
```

As you can see, we extended null-safety to length-safety by introducing a length-indexed vector.
