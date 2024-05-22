# Expressing monoidal category in Haskell, part 1

In Haskell, we have [`Monoid`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#t:Monoid) type class to express monoid. This can be seen as a monoid object in a monoidal category in category `Hask` where bifunctor is `(,)` and its unit is `()`.

In this post, we'll see how we can express a monoidal category and a monoid object themselves in Haskell.

First, let's define some kinds. We use `FunctorType` for a type constructor taking one type, and `BifunctorType` for a type constructor taking two types. We expect these types implement [`Functor`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#t:Functor) and [`Bifunctor`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Bifunctor.html) respectively.

```
{-# LANGUAGE AllowAmbiguousTypes,
             TypeFamilies,
             UndecidableInstances
#-}

import Data.Bifunctor
import Data.Kind
import Data.Void

type FunctorType :: Type
type FunctorType = Type -> Type

type BifunctorType :: Type
type BifunctorType = Type -> Type -> Type
```

To define a monoidal category, we need a category, a bifunctor, and its unit. We've already chosen `Hask` as a category. So all we need are a bifunctor and its unit.

This bifunctor has to satisfy some conditions. When you pick `(,)` as a bifunctor and `()` as its unit, we can express these conditions as follows.

* `(a, (b, c)) ≅ ((a, b), c)`
* `((), a) ≅ a`
* `(a, ()) ≅ a`

In Haskell, we'll express these isomorphisms as these functions

* `assoc :: (a, (b, c)) -> ((a, b), c)`
* `assocInv :: ((a, b), c) -> (a, (b, c))`
* `left :: ((), a) -> a`
* `leftInv :: a -> ((), a)`
* `right :: (a, ()) -> a`
* `rightInv :: a -> (a, ())`

where `assoc . assocInv = assocInv . assoc = id`, `left . leftInv = leftInv . left = id`, `right . rightInv = rightInv . right = id`.

Let's expand it to any bifunctor and make it a type class.

```
type MonoidalCategory :: BifunctorType -> Constraint
class (Bifunctor t) => MonoidalCategory t where
  type Unit t :: Type

  assoc :: t a (t a a) -> t (t a a) a
  assocInv :: t (t a a) a -> t a (t a a)

  left :: t (Unit t) a -> a
  leftInv :: a -> t (Unit t) a

  right :: t a (Unit t) -> a
  rightInv :: a -> t a (Unit t)
```

We can say that a bifunctor implements this type class satisfying the conditions is a monoidal category. Of course, `(,)` can be an instance of this type class.

```
instance MonoidalCategory (,) where
  type Unit (,) = ()

  assoc :: (a, (b, c)) -> ((a, b), c)
  assoc (a, (b, c)) = ((a, b), c)

  assocInv :: ((a, b), c) -> (a, (b, c))
  assocInv ((a, b), c) = (a, (b, c))

  left :: ((), a) -> a
  left ((), a) = a

  leftInv :: a -> ((), a)
  leftInv a = ((), a)

  right :: (a, ()) -> a
  right (a, ()) = a

  rightInv :: a -> (a, ())
  rightInv a = (a, ())
```

In the monoidal category of `(,)` and `()`, we can express monoid objects as objects that have these two operations.

* `mu :: (a, a) -> a`
* `eta :: () -> a`

We'll have this `MonoidObject` type class by expanding this to any bifunctor. We use multi-param type classes instead of type families so that one type can be monoidal objects in different monoidal categories.

```
type MonoidObject :: BifunctorType -> Type -> Constraint
class (MonoidalCategory t) => MonoidObject t a where
  mu :: t a a -> a
  eta :: Unit t -> a
```

For example, we can define some monoidal objects in this monoidal category.

```
instance MonoidObject (,) Int where
  mu :: (Int, Int) -> Int
  mu (n, m) = n + m

  eta :: () -> Int
  eta () = 0

instance MonoidObject (,) [a] where
  mu :: ([a], [a]) -> [a]
  mu (a1, a2) = a1 ++ a2

  eta :: () -> [a]
  eta () = []

instance MonoidObject (,) (a -> a) where
  mu :: (a -> a, a -> a) -> (a -> a)
  mu (f, g) = g . f

  eta :: () -> (a -> a)
  eta () = id
```

Now, let's give another monoidal category a look. We use `Hask` as its category, and use `Either` and `Void` as its bifunctor and its unit.

```
instance MonoidalCategory Either where
  type Unit Either = Void

  assoc :: Either a (Either b c) -> Either (Either a b) c
  assoc (Left a) = Left (Left a)
  assoc (Right (Left b)) = Left (Right b)
  assoc (Right (Right c)) = Right c

  assocInv :: Either (Either a b) c -> Either a (Either b c)
  assocInv (Left (Left a)) = Left a
  assocInv (Left (Right b)) = Right (Left b)
  assocInv (Right c) = Right (Right c)

  left :: Either Void a -> a
  left (Left v) = absurd v
  left (Right a) = a

  leftInv :: a -> Either Void a
  leftInv = Right

  right :: Either a Void -> a
  right (Left a) = a
  right (Right v) = absurd v

  rightInv :: a -> Either a Void
  rightInv = Left
```

In this monoidal category, all types are monoid objects, and it's not very fun. Still, this means that we can have monoidal categories other than `(,)` and `()` in Haskell.

```
instance MonoidObject Either a where
  mu :: Either a a -> a
  mu (Left a) = a
  mu (Right a) = a

  eta :: Void -> a
  eta = absurd
```

Getting back to the first monoidal category (`Hask`, `(,)` and `()`), we can derive the standard `Monoid` (and `Semigroup`) from it.

```
instance {-# OVERLAPPABLE #-} (MonoidObject (,) a) => Semigroup a where
  (<>) :: a -> a -> a
  (<>) a1 a2 = mu (a1, a2)

instance {-# OVERLAPPABLE #-} (MonoidObject (,) a) => Monoid a where
  mempty :: a
  mempty = eta @(,) ()
```

You can see `mappend 1 2 :: Int` is evaluated to `3` through `MonoidObject`, for example. Note that it'll not evaluated to `3` without `MonoidObject` because `Int` isn't an instance of `Monoid` directly. We need [`Data.Monoid.Sum`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Monoid.html#t:Sum) wrapper for it, and make it `getSum $ mappend (Sum 1) (Sum 2)` to get `3`.

In [the next post](./monoidal_category2.html), we'll do the same thing in a category of functors (`Hask^Hask`).
