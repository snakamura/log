# Strong and closed profunctors

We saw strong and closed functors in [Strong and closed functors](../2/strong_closed_functors.html). Then, what will strong and closed profunctors look like?

Strong functors allow you to lift a value into a functor using a tensor (A product for `ProductStrongFunctor`, and a coproduct for `CoproductStrongFunctor`).

```
class (Functor f) => ProductStrongFunctor f where
  strength :: (a, f b) -> f (a, b)

class (Functor f) => CoproductStrongFunctor f where
  strength :: Either a (f b) -> f (Either a b)
```

Strong profunctors allow you to lift a value into a profunctor using a tensor. But this time, this value will be given as a part of an input of the profunctor. Let's see how it'll look like when you use a product as a tensor.

```
class (Profunctor p) => ProductStrongProfunctor p where
  first :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)
```

For example, you can implement `ProductStrongProfunctor` for `(->)`.

```
instance ProductStrongProfunctor (->) where
  first :: (a -> b) -> ((a, c) -> (b, c))
  first a2b (a, c) = (a2b a, c)

  second :: (a -> b) -> ((c, a) -> (c, b))
  second a2b (c, a) = (c, a2b a)
```

What about other profunctors? Let's define `Profunctor` for `a -> f b` where `f` is `Functor`.

```
newtype Star f a b = Star (a -> f b)

instance (Functor f) => Profunctor (Star f) where
  dimap :: (s -> a) -> (b -> t) -> (Star f a b -> Star f s t)
  dimap s2a b2t (Star a2fb) =
    Star
      ( \s ->
          let a = s2a s
              fb = a2fb a
              ft = fmap b2t fb
           in ft
      )
```

Will `Star f` be an instance of `ProductStrongProfunctor`? Yes, as long as `f` is `ProductStrongFunctor`.

```
instance (ProductStrongFunctor f) => ProductStrongProfunctor (Star f) where
  first :: Star f a b -> Star f (a, c) (b, c)
  first (Star a2fb) = Star (\ac -> fmap swap (strength (fmap a2fb (swap ac))))

  second :: Star f a b -> Star f (c, a) (c, b)
  second (Star a2fb) = Star (\ca -> strength $ fmap a2fb ca)
```

Since every functor in Haskell is an instance of `ProductStrongFunctor`, you can say that `Star f` is `ProductStrongProfunctor` with any `Functor` `f`.

As with strong functors, we can define strong profunctors in terms of coproduct.

```
class (Profunctor p) => CoproductStrongProfunctor p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)
```

`(->)` is an instance of this typeclass.

```
instance CoproductStrongProfunctor (->) where
  left :: (a -> b) -> (Either a c -> Either b c)
  left a2b (Left a) = Left (a2b a)
  left _ (Right c) = Right c

  right :: (a -> b) -> (Either c a -> Either c b)
  right _ (Left c) = Left c
  right a2b (Right a) = Right (a2b a)
```

Also, `Star f` is an instance of `CoproductStrongProfunctor` as long as `f` is `CoproductStrongFunctor`.

```
instance (CoproductStrongFunctor f) => CoproductStrongProfunctor (Star f) where
  left :: Star f a b -> Star f (Either a c) (Either b c)
  left (Star a2fb) = Star (\eac -> fmap swapEither (strength (fmap a2fb (swapEither eac))))

  right :: Star f a b -> Star f (Either c a) (Either c b)
  right (Star a2fb) = Star (\eca -> strength (fmap a2fb eca))
```

A functor is `CoproductStrongFunctor` when it's `Applicative` as we saw in [the previous post](../2/strong_closed_functors.html). It means that you can say that `Star f` is an instance of `CoproductStrongProfunctor` if `f` is `Applicative`.

By the way, in Haskell, `second` can be implemented in terms of `first` and vice versa, and `right` can be implemented in terms of `left` and vice versa, by swapping their elements. But this isn't always true when you think about strong profunctors using a asymmetric tensor product.

In `profunctors` package, `ProductStrongProfunctor` is called [`Strong`](https://hackage-content.haskell.org/package/profunctors-5.6.3/docs/Data-Profunctor-Strong.html#t:Strong), and `CoproductStrongProfunctor` is called [`Choice`](https://hackage-content.haskell.org/package/profunctors-5.6.3/docs/Data-Profunctor-Choice.html#t:Choice).

What about closed profunctors? As you might've expected, we can define this `ClosedProfunctor`.

```
class (Profunctor p) => ClosedProfunctor p where
  closed :: p a b -> p (c -> a) (c -> b)
```

Again, `(->)` is an instance of this typeclass.

```
instance ClosedProfunctor (->) where
  closed :: (a -> b) -> ((c -> a) -> (c -> b))
  closed a2b c2a c =
    let a = c2a c
        b = a2b a
     in b
```

Also, `Star f` is an instance of `ClosedProfunctor` if `f` is `ClosedFunctor`.

```
class (Functor f) => ClosedFunctor f where
  closed :: (a -> f b) -> f (a -> b)
```

```
instance (ClosedFunctor f) => ClosedProfunctor (Star f) where
  closed :: forall a b c. Star f a b -> Star f (c -> a) (c -> b)
  closed (Star a2fb) = Star g
    where
      g :: (c -> a) -> f (c -> b)
      g c2a = Functor.Closed.closed h
        where
          h :: c -> f b
          h c = a2fb (c2a c)
```

When you think about a dual of `Star`, we can make `f a -> b` an instance of `Profunctor` and we call it `Costar`.

```
newtype Costar f a b = Costar (f a -> b)

instance (Functor f) => Profunctor (Costar f) where
  dimap :: (s -> a) -> (b -> t) -> (Costar f a b -> Costar f s t)
  dimap s2a b2t (Costar fa2b) =
    Costar
      ( \fs ->
          let fa = fmap s2a fs
              b = fa2b fa
              t = b2t b
           in t
      )
```

`Costar f` is an instance of `ClosedProfunctor` if `f` is a functor.

```
instance (Functor f) => ClosedProfunctor (Costar f) where
  closed :: forall a b c. Costar f a b -> Costar f (c -> a) (c -> b)
  closed (Costar fa2b) = Costar g
    where
      g :: f (c -> a) -> (c -> b)
      g fc2a c = fa2b (fmap h fc2a)
        where
          h :: (c -> a) -> a
          h c2a = c2a c
```
