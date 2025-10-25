# A parameter type is existential, a return type is universal, part 4

We saw an adjunction of `SomeF` and `Const` in [the part 1](../6/existential_universal1.html), and an adjunction of `Const` and `AnyF` in [the part 2](../6/existential_universal2.html). They're called an adjoint triple and denoted as $SomeF \dashv Const \dashv AnyF$. Let's express them as code in Haskell.

We'll use two categories. First category is $Hask$ where objects are types and morphisms are functions. The second category is $Hask^{Hask}$ where objects are endofunctors in $Hask$ and morphisms are natural transformations.

First, let's define some types to express endofunctors in $Hask$ and natural transformations.

```
type FunctorType :: Type
type FunctorType = Type -> Type

type (~>) :: FunctorType -> FunctorType -> Type
type f ~> g = forall a. f a -> g a
```

Then, we'll define two functors. The first functor is a functor from $Hask$ to $Hask^{Hask}$. We'll call it `FunctorFromHaskToHask2`.

```
type FunctorFromHaskToHask2Type :: Type
type FunctorFromHaskToHask2Type = Type -> FunctorType

type FunctorFromHaskToHask2 :: FunctorFromHaskToHask2Type -> Constraint
class FunctorFromHaskToHask2 t where
  mapFromHaskToHask2 ::
    (Functor (t a), Functor (t b)) =>
    (a -> b) -> (t a ~> t b)
```

This functor maps an object in $Hask$ (`Type`) to an object in $Hask^{Hask}$ (`FunctorType`), and maps a morphism in $Hask$ (`Type -> Type`) to a morphism in $Hask^{Hask}$ (`FunctorType ~> FunctorType`).

The second functor is the opposite; $Hask^{Hask}$ to $Hask$. We'll call it `FunctorFromHask2ToHask`.

```
type FunctorFromHask2ToHaskType :: Type
type FunctorFromHask2ToHaskType = FunctorType -> Type

type FunctorFromHask2ToHask :: FunctorFromHask2ToHaskType -> Constraint
class FunctorFromHask2ToHask t where
  mapFromHask2ToHask ::
    (Functor f, Functor g) =>
    (f ~> g) -> (t f -> t g)
```

This functor maps an object in $Hask^{Hask}$ (`FunctorType`) to an object in $Hask$ (`Type`), and maps a morphism in $Hask^{Hask}$ (`FunctorType ~> FunctorType`) to a morphism in $Hask$ (`Type -> Type`).

When you have `Const`,

```
type Const :: Type -> Type -> Type
newtype Const a b = MkConst a
```

you can make `Const a` an instance of `Functor`.

```
instance Functor (Const a) where
  fmap :: (b -> c) -> Const a b -> Const a c
  fmap _ (MkConst a) = MkConst a
```

When you look at the kind of `Const`, you'll find its type is `Type -> Type -> Type`, but it's identical to `Type -> FunctorType`. So you can think `Const` is a functor from $Hask$ to $Hask^{Hask}$. Let's make it an instance of `FunctorFromHaskToHask2`.

```
instance FunctorFromHaskToHask2 Const where
  mapFromHaskToHask2 :: (a -> b) -> (Const a ~> Const b)
  mapFromHaskToHask2 a2b = \(MkConst a) -> MkConst (a2b a)
```

Next, let's take a look at `SomeF`.

```
type SomeF :: FunctorType -> Type
data SomeF f = forall a. MkSomeF (f a)
```

`SomeF` is a type that takes a `FunctorType`. So you can think it's a functor from $Hask^{Hask}$ to $Hask$. Let's make it an instance of `FunctorFromHask2ToHask`.

```
instance FunctorFromHask2ToHask SomeF where
  mapFromHask2ToHask :: (f ~> g) -> (SomeF f -> SomeF g)
  mapFromHask2ToHask f2g = \(MkSomeF fa) -> MkSomeF (f2g fa)
```

You can do the same with `AnyF`.

```
type AnyF :: FunctorType -> Type
newtype AnyF f = MkAnyF (forall a. f a)

instance FunctorFromHask2ToHask AnyF where
  mapFromHask2ToHask :: (f ~> g) -> (AnyF f -> AnyF g)
  mapFromHask2ToHask f2g = \(MkAnyF fa) -> MkAnyF (f2g fa)
```

Now, we've had necessary functors; `Const` from $Hask$ to $Hask^{Hask}$, and `SomeF` and `AnyF` from $Hask^{Hask}$ to $Hask$. We'll define two type classes expressing adjunctions. The first one has $Hask^{Hask}$ to $Hask$ on the left, and $Hask$ to $Hask^{Hask}$ on the right.

```
class
  (FunctorFromHask2ToHask f, FunctorFromHaskToHask2 g) =>
  LeftAdjunction f g
    | f -> g,
      g -> f
  where
  leftLeftAdjunct :: (Functor h) => (f h -> a) -> (h ~> g a)
  rightLeftAdjunct :: (Functor h) => (h ~> g a) -> (f h -> a)
```

The second one has the opposite.

```
class
  (FunctorFromHaskToHask2 f, FunctorFromHask2ToHask g) =>
  RightAdjunction f g
    | f -> g,
      g -> f
  where
  leftRightAdjunct :: (Functor h) => (f a ~> h) -> (a -> g h)
  rightRightAdjunct :: (Functor h) => (a -> g h) -> (f a ~> h)
```

We can make a pair of `SomeF` and `Const` an instance of `LeftAdjunction`, and a pair of `Const` and `AnyF` an instance of `RightAdjunction`.

```
instance LeftAdjunction SomeF Const where
  leftLeftAdjunct :: (Functor h) => (SomeF h -> a) -> (h ~> Const a)
  leftLeftAdjunct someH2a = \h -> MkConst (someH2a (MkSomeF h))

  rightLeftAdjunct :: (Functor h) => (h ~> Const a) -> (SomeF h -> a)
  rightLeftAdjunct h2const = \(MkSomeF h) -> let MkConst a = h2const h in a

instance RightAdjunction Const AnyF where
  leftRightAdjunct :: (Functor h) => (Const a ~> h) -> (a -> AnyF h)
  leftRightAdjunct const2h = \a -> MkAnyF (const2h (MkConst a))

  rightRightAdjunct :: (Functor h) => (a -> AnyF h) -> (Const a ~> h)
  rightRightAdjunct a2anyH = \(MkConst a) -> let MkAnyF ha = a2anyH a in ha
```

In an adjoint triple, there is a forgetful functor in the middle, and a free functor on the left and a cofree functor on the right. A forgetful functor forgets a structure, a free functor creates a minimum structure, and a cofree functor creates a maximum structure.

In this adjoint triple $SomeF \dashv Const \dashv AnyF$, you can think that `Const` forgets a functor `f`, `SomeF` creates a minimum functor that works with some given `a`, and `AnyF` creates a maximum functor that works with any given `a`.
