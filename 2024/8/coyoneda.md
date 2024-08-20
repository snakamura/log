# Coyoneda from Yoneda

We defined `forward` and `backward` in [the previous post](./yoneda.html).

```
forward :: forall f a. (forall x. (a -> x) -> f x) -> f a
forward f = f id

backward :: forall f a. Functor f => f a -> (forall x. (a -> x) -> f x)
backward v = \f -> fmap f v
```

The type of `backward` can be expanded to `backward :: forall f a x. Functor f => f a -> (a -> x) -> f x`. Now, let's uncurry `backward`.

```
backward' :: forall f a x. Functor f => (f a, a -> x) -> f x
backward' (v, f) = fmap f v
```

Then, flip `a` and `x` in the type signature to align types with `forward`.

```
backward'' :: forall f a x. Functor f => (f x, x -> a) -> f a
backward'' (v, f) = fmap f v
```

With this type definition, the entire `backward''` is polymorphic to `x`, but `x` doesn't need to be polymorphic over the entire `backward''`. We `fmap` `f` on `v`, so `x` only needs to be polymorphic over `(f x, x -> a)`. Unfortunately, we cannot write it `forall f a. Functor f => (forall x. (f x, x -> a)) -> f a`, and need an explicit existential type.

```
data X f a = forall x. X (f x, x -> a)

backward''' :: forall f a. Functor f => X f a -> f a
backward''' (X (v, f)) = fmap f v
```

Next, let's flip the argument and the return value to get a reverse function.

```
forward' :: f a -> X f a
forward' v = X (v, id)
```

With `backward'''` and `forward'`, we can say they represent an isomorphism between `f a` and `X f a` which is derived from Yoneda lemma.

In [Data.Functor.Coyoneda](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Coyoneda.html), `X` is called `Coyoneda` (by flipping its components and removing the redundant tuple). `forward'` is called `liftCoyoneda`, and `backward'''` is called `lowerCoyoneda`.

```
type Coyoneda :: (Type -> Type) -> Type -> Type
data Coyoneda f a = forall b. Coyoneda (b -> a) (f b)

liftCoyoneda :: f a -> Coyoneda f a
liftCoyoneda = Coyoneda id

lowerCoyoneda :: Functor f => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda f v) = fmap f v
```
