# Monoid of functors

As we saw in [the previous post](https://snak.tumblr.com/post/679221762414837760/when-do-you-use-datamonoidendo), a function from `a` to `a` forms a monoid with respect to their composition and an identity function. Then, what we can do with a functor?

```
{-# LANGUAGE FlexibleInstances,
             RankNTypes,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
import Data.Kind (Type)
import Data.Monoid (Endo(Endo))
```

First, let's revisit the monoid instance of `Endo`. But this time, we define our monoid class.

```
class Monoid' a where
    mu :: (a, a) -> a
    eta :: () -> a
```

This class expresses the same idea as the standard `Monoid`. `mu` is an uncurried version of `mappend`, and `eta` is a function version of `mempty`. You can implement an instance for `Endo` easily.

```
instance Monoid' (Endo a) where
    mu (Endo f, Endo g) = Endo $ f . g
    eta () = Endo id
```

Actually, we don't necessarily use a tuple and a unit, but can use some kind of product and its unit if they satisfy certain laws, such as the product is a bifunctor. You can find more details at [From Monoids to Monads](http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html), and [Monads Categorically](https://bartoszmilewski.com/2016/12/27/monads-categorically/). I'll skip these details in this article, but focus on get some intuitions from the code.

So, let's factor out these types using type families.

```
class Monoid'' a where
    type Product'' a
    type Unit'' a
    mu' :: Product'' a -> a
    eta' :: Unit'' a -> a

instance Monoid'' (Endo a) where
    type Product'' (Endo a) = (Endo a, Endo a)
    type Unit'' (Endo a) = ()
    mu' (Endo f, Endo g) = Endo $ f . g
    eta' () = Endo id
```

Then, now we're going to define a class for functors.

```
class MonoidF' f where
    type ProductF' (f :: Type -> Type) :: Type -> Type
    type UnitF' f :: Type -> Type
    muF' :: ProductF' f a -> f a
    etaF' :: UnitF' f a -> f a
```

This looks very similar to `Monoid''` except that it's for functors. By introducing an operator `(~>)`, they can look even similar.

```
type f ~> g = forall a. f a -> g a

class MonoidF'' f where
    type ProductF'' (f :: Type -> Type) :: Type -> Type
    type UnitF'' f :: Type -> Type
    muF'' :: ProductF'' f ~> f
    etaF'' :: UnitF'' f ~> f
```

You can write instances of this class for, for example, `Maybe`, `(->) r`, and `(,) w`.

```
instance MonoidF'' Maybe where
    type ProductF'' Maybe = Compose Maybe Maybe
    type UnitF'' Maybe = Identity
    muF'' (Compose (Just (Just x))) = Just x
    muF'' _ = Nothing
    etaF'' (Identity x) = Just x

instance MonoidF'' ((->) r) where
    type ProductF'' ((->) r) = Compose ((->) r) ((->) r)
    type UnitF'' ((->) r) = Identity
    muF'' (Compose f) e = f e e
    etaF'' (Identity x) = const x

instance Monoid w => MonoidF'' ((,) w) where
    type ProductF'' ((,) w) = Compose ((,) w) ((,) w)
    type UnitF'' ((,) w) = Identity
    muF'' (Compose (w2, (w1, x))) = (w1  w2, x)
    etaF'' (Identity x) = (mempty, x)
```

These functors form a monoid with respect to their composition and an identity functor just like `Endo` forms a monoid.

As you may have noticed, it's identical to a monad. Let's define our monad class.

```
class Monad' f where
    join' :: f (f a) -> f a
    pure' :: a -> f a
```

Instead of defining it with `(>>=)` and `pure`, I defined it with `join` and `pure` because it's easier to write. You can write an instance of this class for functors that are an instance of `MonoidF''`.

```
instance (MonoidF'' f, ProductF'' f ~ Compose f f, UnitF'' f ~ Identity) => Monad' f where
    join' = muF'' . Compose
    pure' = etaF'' . Identity
```

So you can think that a functor is a monad if its composition forms a monoid with respect to its composition and an identity functor.

Note that you can implement the standard `Applicative` and `Monad` like this.

```
instance (Functor f, MonoidF'' f, ProductF'' f ~ Compose f f, UnitF'' f ~ Identity) => Applicative f where
    (<*>) f m = muF'' $ Compose $ fmap (\f' -> muF'' $ Compose $ fmap (pure . f') m) f
    pure = etaF'' . Identity

instance (Functor f, MonoidF'' f, ProductF'' f ~ Compose f f, UnitF'' f ~ Identity) => Monad f where
    (>>=) m f = muF'' $ Compose $ fmap f m
```
