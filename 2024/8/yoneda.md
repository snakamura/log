# Yoneda lemma in Haskell

Yoneda lemma says

> Let `F` be a functor from a locally small category `C` to `Set`. Then for each object `A` of `C`, the natural transformations <code>Nat(h<sub>A</sub>, F) ≡ Hom(Hom(A, −), F)</code> from <code>h<sub>A</sub></code> to `F` are in one-to-one correspondence with the elements of `F(A)`.

What does it mean in Haskell?

First, we use `Hask` the category of Haskell types as `C` and `Set`. Then `F` will be a normal Haskell `Functor`.

`Hom(A, -)` is a hom functor. It's `(->) A` when you write it in Haskell. Yes, it's a reader functor. A natural transformation is written as a polymorphic function in Haskell. You can write a natural transformation from a hom functor to functor `F` in Haskell like this.

```
nat :: forall x. ((->) A) x -> F x
```

whose type can be expanded to this.

```
nat :: forall x. (A -> x) -> F x
```

Let's use `Bool` as `A`, and `Maybe` as `F` as examples. Then you'll get this function.

```
nat :: forall x. (Bool -> x) -> Maybe x
```

How many functions of this type do you come up with? There are three.

```
nat1, nat2, nat3 :: forall x. (Bool -> x) -> Maybe x
nat1 f = Just (f True)
nat2 f = Just (f False)
nat3 _ = Nothing
```

To return `Just`, we need a value of `x`. To get a value of `x`, we need to call `f` by passing `True` or `False`. Otherwise, we'll return `Nothing`. In other words, we can select up to three values of `Maybe x` no matter what `x` is.

Yoneda lemma says `nat` is isomorphic to `Maybe Bool`. Indeed, we have three values of `Maybe Bool`; `Just True`, `Just False` and `Nothing`. We can define functions to convert them back and forth.

```
forward :: (forall x. (Bool -> x) -> Maybe x) -> Maybe Bool
forward f = f id

backward :: Maybe Bool -> (forall x. (Bool -> x) -> Maybe x)
backward v = \f -> fmap f v
```

For example, `forward nat1 = Just (id True) = Just True`, and `backward (Just True) = \f -> fmap f (Just True) = \f -> Just (f True) = nat1`.

This means that they are isomorphic.

When you look at these two functions, you can find they work not only with `Bool` and `Maybe`, but with any type `a` and any `Functor` `f`.

```
forward :: forall f a. (forall x. (a -> x) -> f x) -> f a
forward f = f id

backward :: forall f a. Functor f => f a -> (forall x. (a -> x) -> f x)
backward v = \f -> fmap f v
```

In [`Data.Functor.Yoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Yoneda.html), `(a -> x) -> f x` is newtype'd as `Yoneda`, and `forward` is called `lowerYoneda` and `backward` is called `liftYoneda`.

```
type Yoneda :: (Type -> Type) -> Type -> Type
newtype Yoneda f a = Yoneda (forall x. (a -> x) -> f x)

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda f) = f id

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda v = Yoneda (\f -> fmap f v)
```
