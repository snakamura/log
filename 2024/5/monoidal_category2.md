# Expressing monoidal category in Haskell, part 2

In [the previous post](./monoidal_category1.html), we saw two monoidal categories in `Hask`. This time, we'll see some monoidal categories in `Hask^Hask`. `Hask^Hask` is a category of functors in Haskell. Objects are functors and morphisms are natural transformations in this category.

When we saw monoidal categories in `Hask`, we used standard `Functor` and `Bifunctor` to express functors and bifunctors in it. This time, we'll define `Functor2` and `Bifunctor2` by ourselves to express functors and bifunctors in `Hask^Hask`.

First, we'll define `~>` to express morpihsms in `Hask^Hask`. In `Hask`, `->` expresses a morphism which is a function. In `Hask^Hask`, morphisms are natural transformations and are expressed as polymorphic functions `f a -> g a` for all `a`. We'll write it as `f ~> g` to make it look more like a function.

```
{-# LANGUAGE AllowAmbiguousTypes,
             QuantifiedConstraints,
             TypeFamilies,
             UndecidableInstances
#-}

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Day
import Data.Functor.Identity
import Data.Functor.Product
import Data.Kind
import Data.Proxy

type FunctorType :: Type
type FunctorType = Type -> Type

type (~>) :: FunctorType -> FunctorType -> Type
type f ~> g = forall a. f a -> g a
```

Next, just like we defined `FunctorType` in the previous post, we'll define `Functor2Type` which is a function from a functor to a functor. It means that `Functor2` maps a functor (`FunctorType`) to a functor (`FunctorType`). A functor is an object in `Hask^Hask`, so `Functor2Type` maps an object to an object in `Hask^Hask`.

```
type Functor2Type :: Type
type Functor2Type = FunctorType -> FunctorType
```

Actual functors need to be an instance of this `Functor2` type class. Here, `fmap2` maps a natural transformation (`(~>)`) to a natural transformation (`(~>)`). Since natural transformations are morphisms in `Hask^Hask`, you can say that `fmap2` maps a morphism to a morphism in `Hask^Hask`.

```
type Functor2 :: Functor2Type -> Constraint
class Functor2 t where
  fmap2 :: (Functor f, Functor g) => (f ~> g) -> (t f ~> t g)
```

We'll do the same for bifunctors to have `Bifunctor2Type` and `Bifunctor` type class.

```
type Bifunctor2Type :: Type
type Bifunctor2Type = FunctorType -> FunctorType -> FunctorType

type Bifunctor2 :: Bifunctor2Type -> Constraint
class (forall f. (Functor f) => Functor2 (t f)) => Bifunctor2 t where
  bimap2 ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (t f g ~> t h i)
```

These types and type classes are functors (`FunctorType` and `Functor`) and bifunctors (`BifunctorType` and `Bifunctor`) lifted to `Hask^Hask`. When you compare them with `Functor` and `Bifunctor`, you can find that they look very similar except that `Functor2` and `Bifunctor2` use `~>` instead of `->`. This is so because morphisms in `Hask` are functions (`->`) while morphisms in `Hask^Hask` are natural transformations (`~>`).

Now let's define type classes for monoidal categories and monoidal objects just like we did in the previous post.

As we saw there, we need a category, bifunctor and its unit to define a monoidal category. This time, a category is `Hask^Hask`. A bifunctor is `Bifunctor2` that satisfies some conditions.

When `f`, `g` and `h` are objects in `Hask^Hask` (so they're functors in `Hask`), we can express these conditions as

- ``f `bif2` (g `bif2` h) ≅ (f `bif2` g) `f2` h``
- ``f `bif2` u ≅ f``
- ``u `bif2` f ≅ f``

where `bif2` is a bifunctor, and `u` is its unit. For example, when you use `Compose` as a bifunctor and `Identity` as its unit, they mean these conditions.

- `Compose f (Compose g h) ≅ Compose (Compose f g) h`
- `Compose f Identity ≅ f`
- `Compose Identity f ≅ f`

Let's express them as `FunctorMonoidalCategory` type class. We'll express an ismorphism as two bidirectional natural transformations (morphisms) such as `assoc` and `assocInv` where `assoc . assocInv = assocInv . assoc = id`.

```
type FunctorMonoidalCategory :: Bifunctor2Type -> Constraint
class (Bifunctor2 t, Functor (FunctorUnit t)) => FunctorMonoidalCategory t where
  type FunctorUnit t :: FunctorType

  assoc :: (Functor f, Functor g, Functor h) => t f (t g h) ~> t (t f g) h
  assocInv :: (Functor f, Functor g, Functor h) => t (t f g) h ~> t f (t g h)

  left :: (Functor f) => t (FunctorUnit t) f ~> f
  leftInv :: (Functor f) => f ~> t (FunctorUnit t) f

  right :: (Functor f) => t f (FunctorUnit t) ~> f
  rightInv :: (Functor f) => f ~> t f (FunctorUnit t)
```

In this monoidal category, we can express a monoid object as an object that has these two operations.

* ``mu :: `f `bif2` f ~> f``
* `eta :: u ~> f`

We'll express these two in `FunctorMonoidObject` type class.

```
type FunctorMonoidObject :: Bifunctor2Type -> FunctorType -> Constraint
class (FunctorMonoidalCategory t, Functor f) => FunctorMonoidObject t f where
  mu :: t f f ~> f
  eta :: FunctorUnit t ~> f
```

This construct has the same structure as `MonoidalCategory` and `MonoidObject` in the previous post, but they're lifted to `Hask^Hask`.

Let's take an example. In this example, we'll use [`Compose`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Data-Functor-Compose.html#t:Compose) as a bifunctor, and [`Identity`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Data-Functor-Identity.html#t:Identity) as its unit. First, let's make `Compose` an instance of `Functor2` and `Bifunctor2`.

```
instance (Functor f) => Functor2 (Compose f) where
  fmap2 ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Compose f g ~> Compose f h)
  fmap2 gh (Compose fga) = Compose (fmap gh fga)

instance
  (forall f. (Functor f) => Functor2 (Compose f)) =>
  Bifunctor2 Compose
  where
  bimap2 ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Compose f g ~> Compose h i)
  bimap2 fh gi (Compose fga) =
    let fia = fmap gi fga
     in Compose (fh fia)
```

With them, you can make it an instance of `FunctorMonoidalCategory`.

```
instance FunctorMonoidalCategory Compose where
  type FunctorUnit Compose = Identity

  assoc ::
    (Functor f, Functor g, Functor h) =>
    Compose f (Compose g h) ~> Compose (Compose f g) h
  assoc (Compose fcgha) =
    let fgha = fmap (\(Compose gha) -> gha) fcgha
     in Compose (Compose fgha)

  assocInv ::
    (Functor f, Functor g, Functor h) =>
    Compose (Compose f g) h ~> Compose f (Compose g h)
  assocInv (Compose (Compose fgh)) = Compose (fmap Compose fgh)

  left :: (Functor f) => Compose Identity f ~> f
  left (Compose (Identity f)) = f

  leftInv :: f ~> Compose Identity f
  leftInv f = Compose (Identity f)

  right :: (Functor f) => Compose f Identity ~> f
  right (Compose fia) = fmap (\(Identity a) -> a) fia

  rightInv :: (Functor f) => f ~> Compose f Identity
  rightInv fa = Compose (fmap Identity fa)
```

You can make, for example, `Maybe` an instance of `FunctorMonoidObject` with `FunctorMonoidalCategory`. This means that `Maybe` is a monoid object in the monoidal category that consists of `Hask^Hask`, `Compose` and `Identity`.

```
instance FunctorMonoidObject Compose Maybe where
  mu :: Compose Maybe Maybe ~> Maybe
  mu (Compose (Just (Just a))) = Just a
  mu (Compose _) = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a
```

You can make a list (`[]`) an instance as well.

```
instance FunctorMonoidObject Compose [] where
  mu :: Compose [] [] ~> []
  mu (Compose a) = concat a

  eta :: Identity ~> []
  eta (Identity a) = [a]
```

As we saw in [Monoid of functors](../../2022/3/monoid_functor.html), this monoid object is identical to a monad. Actually, we can derive an instance of `Applicative` and `Monad` from `FunctorMonoidObject`.

```
instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Compose f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa =
    mu $
      Compose $
        fmap
          ( \ab ->
              mu $ Compose $ fmap (pure . ab) fa
          )
          fab

  pure :: a -> f a
  pure = eta @Compose . Identity

instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Compose f
  ) =>
  Monad f
  where
  (>>=) :: f a -> (a -> f b) -> f b
  (>>=) fa afb = mu $ Compose $ fmap afb fa
```

Let's take another example. This time, we'll use [`Day`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Day.html) as a bifunctor and use `Identity` as its unit again.

First, make `Day` an instance of `Functor2` and `Bifunctor2`.

```
instance (Functor f) => Functor2 (Day f) where
  fmap2 ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Day f g ~> Day f h)
  fmap2 gh (Day f g bca) = Day f (gh g) bca

instance (forall f. (Functor f) => Functor2 (Day f)) => Bifunctor2 Day where
  bimap2 ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Day f g ~> Day h i)
  bimap2 fh gi (Day f g bca) = Day (fh f) (gi g) bca
```

Then, you can make it an instance of `FunctorMonoidalCategory`.

```
instance FunctorMonoidalCategory Day where
  type FunctorUnit Day = Identity

  assoc ::
    (Functor f, Functor g, Functor h) =>
    Day f (Day g h) ~> Day (Day f g) h
  assoc (Day fb (Day gc hd cde) bea) =
    Day (Day fb gc (,)) hd (\(b, c) d -> bea b (cde c d))

  assocInv ::
    (Functor f, Functor g, Functor h) =>
    Day (Day f g) h ~> Day f (Day g h)
  assocInv (Day (Day fb gc bcd) he dea) =
    Day fb (Day gc he (,)) (\b (c, e) -> dea (bcd b c) e)

  left :: (Functor f) => Day Identity f ~> f
  left (Day (Identity b) fc bca) = fmap (bca b) fc

  leftInv :: (Functor f) => f ~> Day Identity f
  leftInv fa = Day (Identity ()) fa (flip const)

  right :: (Functor f) => Day f Identity ~> f
  right (Day fb (Identity c) bca) = fmap (flip bca c) fb

  rightInv :: (Functor f) => f ~> Day f Identity
  rightInv fa = Day fa (Identity ()) const
```

It might not look that `left . leftInv = leftInv . left = id` and `right . rightInv = rightInv . right = id` hold, but you can see `Day Identity f` and `f` are isomorphic in [this post](../4/day_natural_isomorphism.html).

Now we have a category (`Hask^Hask`), a bifunctor (`Day`) and its unit (`Identity`), let's make `Maybe` and `[]` a monoid object in it.

```
instance FunctorMonoidObject Day Maybe where
  mu :: Day Maybe Maybe ~> Maybe
  mu (Day (Just b) (Just c) bca) = Just (bca b c)
  mu _ = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a

instance FunctorMonoidObject Day [] where
  mu :: Day [] [] ~> []
  mu (Day bs cs bca) = [bca b c | b <- bs, c <- cs]

  eta :: Identity ~> []
  eta (Identity a) = [a]
```

This time, this monoid object is identical to `Applicative`. You can implement `Applicative` from this `FunctorMonoidObject`.

```
instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Day f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa = mu $ Day fab fa (\ab a -> ab a)

  pure :: a -> f a
  pure = eta @Day . Identity
```

It's interesting that we can derive `Applicative` both from the first monoidal category (`Hask^Hask`, `Compose` and `Identity`), and the second monoidal category (`Hask^Hask`, `Day` and `Identity`).

Finally, we'll pick one more example. This time, we use [`Product`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Data-Functor-Product.html#t:Product) as a bifunctor and use [`Proxy`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Data-Proxy.html#t:Proxy) as its unit.

Again, we'll first make `Product` an instance of `Functor2` and `Bifunctor2`.

```
instance (Functor f) => Functor2 (Product f) where
  fmap2 ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Product f g ~> Product f h)
  fmap2 gh (Pair fa ga) = Pair fa (gh ga)

instance
  (forall f. (Functor f) => Functor2 (Product f)) =>
  Bifunctor2 Product
  where
  bimap2 ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Product f g ~> Product h i)
  bimap2 fh gi (Pair fa ga) = Pair (fh fa) (gi ga)
```

Then, make it an instance of `FunctorMonoidalCategory` with `Proxy` as its unit.

```
instance FunctorMonoidalCategory Product where
  type FunctorUnit Product = Proxy

  assoc ::
    (Functor f, Functor g, Functor h) =>
    Product f (Product g h)
      ~> Product
           (Product f g)
           h
  assoc (Pair fa (Pair ga ha)) = Pair (Pair fa ga) ha

  assocInv ::
    (Functor f, Functor g, Functor h) =>
    Product (Product f g) h ~> Product f (Product g h)
  assocInv (Pair (Pair fa ga) ha) = (Pair fa (Pair ga ha))

  left :: (Functor f) => Product Proxy f ~> f
  left (Pair Proxy fa) = fa

  leftInv :: (Functor f) => f ~> Product Proxy f
  leftInv fa = Pair Proxy fa

  right :: (Functor f) => Product f Proxy ~> f
  right (Pair fa Proxy) = fa

  rightInv :: (Functor f) => f ~> Product f Proxy
  rightInv fa = Pair fa Proxy
```

Again, `Maybe` and `[]` can be a monoid object in this monoidal category, too.

```
instance FunctorMonoidObject Product Maybe where
  mu :: Product Maybe Maybe ~> Maybe
  mu (Pair (Just a) _) = Just a
  mu (Pair _ (Just a)) = Just a
  mu _ = Nothing

  eta :: Proxy ~> Maybe
  eta _ = Nothing

instance FunctorMonoidObject Product [] where
  mu :: Product [] [] ~> []
  mu (Pair as bs) = as <> bs

  eta :: Proxy ~> []
  eta _ = []
```

As you might have expected, this monoid object is identical to `Alternative` (but the functor has to be `Applicative` because `Alternative` has (theoretically unnecessary) `Applicative` constraint), and you can derive `Alternative` from this monoid object.

```
instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    Applicative f,
    FunctorMonoidObject Product f
  ) =>
  Alternative f
  where
  (<|>) :: f a -> f a -> f a
  (<|>) fa1 fa2 = mu (Pair fa1 fa2)

  empty :: f a
  empty = eta @Product Proxy
```
