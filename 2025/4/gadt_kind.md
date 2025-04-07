# Invisible kinds of GADT can be also generalized

GADT allows you to declare a type whose type parameters are more generic than type of each constructor.

```
data X a where
  XInt :: X Int
```

As you can see, `a` is generalized even though you can only create `X Int` using `XInt`.

This applies to kinds as well even when they're invisible. Let's take an example.

```
data Y f where
  YMaybe :: (a -> Maybe a) -> Y Maybe
```

GHC infers a kind of `Y` as `(Type -> Type) -> Type`. But you can make it more generic by giving it a kind signature.

```
type Y :: (k -> Type) -> Type
data Y f where
  YMaybe :: (a -> Maybe a) -> Y Maybe
```

Now a kind of `f` is `k -> Type` instead of `Type -> Type`. This first looks weird because there is only one data constructor `YMaybe` which returns `Y Maybe` whose kind is `Type -> Type`.

But just like we could make `a` of `X` generalized, we can make a kind of `f` generalized.

When you have type `T` indexed by kind `S`,

```
type data S = S1 | S2

type T :: S -> Type
data T s = T
```

you can add a new data constructor to `Y` where a kind of `f` is `S -> Type`.

```
type Y :: (k -> Type) -> Type
data Y f where
  YMaybe :: (a -> Maybe a) -> Y Maybe
  YT :: T s -> Y T
```
