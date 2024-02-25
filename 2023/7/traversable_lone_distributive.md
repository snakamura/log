# Traversable, Lone and Distributive

`Traversable` type class is a fundamental class in Haskell that most container types are instance of. The most important factor of `Traversable` is `sequenceA`. Let's revisit it by defining our own `Traversable'`.

```
import Control.Applicative
import Data.Functor.Const
import Data.Functor.Identity

class (Functor f) => Traversable' f where
  sequence' :: (Applicative g) => f (g a) -> g (f a)
```

We call it `sequence'` here. As you can see, we have two functors `f` and `g`, and if `f` is `Traversable'`, you can convert `f (g a)` to `g (f a)` with any `Applicative` `g`.

You can make most of containers an instance of `Traversable'` such as `Identity`, `((,) b)`, `Either b` and so on.

```
instance Traversable' Identity where
  sequence' :: (Applicative g) => Identity (g a) -> g (Identity a)
  sequence' (Identity ga) = Identity <$> ga

instance Traversable' Maybe where
  sequence' :: (Applicative g) => Maybe (g a) -> g (Maybe a)
  sequence' Nothing = pure Nothing
  sequence' (Just ga) = Just <$> ga

instance Traversable' [] where
  sequence' :: (Applicative g) => [g a] -> g [a]
  sequence' [] = pure []
  sequence' (ga : gas) = (:) <$> ga <*> sequence' gas

instance Traversable' ((,) b) where
  sequence' :: (Applicative g) => (b, g a) -> g (b, a)
  sequence' (b, ga) = (b,) <$> ga

instance Traversable' (Either b) where
  sequence' :: (Applicative g) => Either b (g a) -> g (Either b a)
  sequence' (Left b) = pure (Left b)
  sequence' (Right ga) = Right <$> ga

instance Traversable' (Const b) where
  sequence' :: (Applicative g) => Const b (g a) -> g (Const b a)
  sequence' (Const b) = pure (Const b)
```

You can find some instances only use `fmap` or `(<$>)` to implement `Traversable'`. For example, `Identity` and `((,) b)` don't use `pure` nor `(<*>)`. So what if we have only `Functor` instead of `Applicative`? Let's define such type class named `Lone`.

```
class (Traversable' f) => Lone f where
  sequenceL :: (Functor g) => f (g a) -> g (f a)
```

`sequenceL` is very similar to `sequence'`, but has `Functor` constraint instead of `Applicative`. The implementations of `sequenceL` are identical to `sequence'`. You'll find a container that always have one element can be an instance of `Lone`.

```
instance Lone Identity where
  sequenceL :: (Functor g) => Identity (g a) -> g (Identity a)
  sequenceL (Identity ga) = Identity <$> ga

instance Lone ((,) b) where
  sequenceL :: (Functor g) => (b, g a) -> g (b, a)
  sequenceL (b, ga) = (b,) <$> ga
```

It turns out that all `Lone` functors are isomorphic to `((,) b)` as it has one element that will be mapped over with additional information `b`. Note that `Identity` is isomorphic to `((,) ())`.

So now, we have two functor `f` and `g`, and if `f` is `Lone`, you can convert `f (g a)` to `g (f a)` with any functor `g`.

Now, let's reverse the direction. What constraint do we need if we apply it to `g` instead of `f`? We can define `Distributive` type class.

```
class (Functor g) => Distributive g where
  distribute :: (Functor f) => f (g a) -> g (f a)
```

`Identity` can be an instance of `Distributive`.

```
instance Distributive Identity where
  distribute :: (Functor f) => f (Identity a) -> Identity (f a)
  distribute gfa = Identity (runIdentity <$> gfa)
```

Also, you can make a function an instance as well.

```
instance Distributive ((->) b) where
  distribute :: (Functor f) => f (b -> a) -> b -> f a
  distribute ff b = (\f -> f b) <$> ff
```

It turns out that all `Distributive` functors are isomorphic to `((->) b)`. Note that `Identity` is isomorphic to `((->) ())`.

So now, we have two functors `f` and `g`, and if `g` is `Distributive`, you can convert `f (g a)` to `g (f a)` with any functor `f`.

There are functors that are instances of both `Lone` and `Distributive`, which are isomorphic to both `((,) b)` and `((->) b)`, that means they're isomorphic to `Identity`.

```
type Identical f = (Lone f, Distributive f)
```
