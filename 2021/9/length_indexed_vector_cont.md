# Implementing length-indexed vector using dependent types (cont.)

We've implemented a length-indexed vector manually in [the previous post](https://snak.tumblr.com/post/662857073524113408/implementing-length-indexed-vector-using-dependent). How will it look like with [singletons](https://hackage.haskell.org/package/singletons)?

We no longer need to define `SNat` manually. Also, by making `Nat` derive `Eq`, singletons makes `SNat` an instance of [`TestEquality`](https://hackage.haskell.org/package/base/docs/Data-Type-Equality.html#t:TestEquality). So we don't need to write `sameSNat`.

```
{-# LANGUAGE DataKinds,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

import Data.Kind (Type)
import Data.Singletons.Prelude (FlipSym1)
import Data.Singletons.Sigma (Sigma((:&:)))
import Data.Singletons.TH
import Data.Type.Equality ((:~:)(Refl), testEquality)

singletons [d|
  data Nat = Z | S Nat deriving (Show, Eq)
  |]

type Nat0 = 'Z
type Nat1 = 'S 'Z
type Nat2 = 'S Nat1
type Nat3 = 'S Nat2
```

The definition of `Vec` is pretty much the same, but we can use `Sigma` instead of our hand-written `SomeVec`.

```
type Vec :: Nat -> Type -> Type
data Vec len a where
    VCons :: a -> Vec len a -> Vec ('S len) a
    VNil :: Vec 'Z a
deriving instance Show a => Show (Vec len a)

type SomeVec a = Sigma Nat (FlipSym1 (TyCon Vec) @@ a)
```

The usages of it are very simillar to what we saw in the previous post.

```
vec :: [a] -> SomeVec a
vec [] = SZ :&: VNil
vec (x:xs) = case vec xs of
               sLen :&: v -> SS sLen :&: VCons x v


sum3 :: Monoid a => Vec Nat3 a -> a
sum3 (VCons a1 (VCons a2 (VCons a3 VNil))) = a1  a2  a3

trySum3 :: Monoid a => [a] -> Maybe a
trySum3 xs = case vec xs of
               (SS (SS (SS SZ))) :&: v -> Just $ sum3 v
               _ -> Nothing


vzip :: Vec len a -> Vec len b -> Vec len (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) $ vzip xs ys

tryZip :: [a] -> [b] -> Maybe (SomeVec (a, b))
tryZip xs ys = case (vec xs, vec ys) of
                 (sLenX :&: vx, sLenY :&: vy) ->
                   case testEquality sLenX sLenY of
                     Just Refl -> Just (sLenX :&: vzip vx vy)
                     Nothing -> Nothing
```
