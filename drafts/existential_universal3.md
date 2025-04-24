# A parameter type is existential, a return type is universal, part 3

In the previous posts, we saw that a parameter type is existential and a return type is universal. Let's take a look at some examples.

The first example is `fmap`. The type of `fmap` is `Functor f => (a -> b) -> f a -> f b`. When you uncurry its parameters, you'll get `Functor f => ((a -> b), f a) -> f b`. `a` appears only in the parameter, so it should be existential. Let's define this existential type.

```
type SomeFA :: (Type -> Type) -> Type -> Type
data SomeFA f a = forall x. MkSomeFA (x -> a) (f x)
```

I renamed `a` to `x` to make it easier to see it's an existential type, and renamed `b` to `a`. When you define `fmap'` like this.

```
fmap' :: Functor f => SomeFA f a -> f a
fmap' = \(MkSomeFA g fa) -> fmap g fa
```

`fmap` and `fmap'` are isomorphic. Note that this `fmap'` is a natural transformation and you can write its type as `Functor f => SomeFA f ~> f`. This means that there should be a natural transformation from `SomeFA f` to `f` if `f` is `Functor`.

This `SomeFA` is identical to [`Coyoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Coyoneda.html#t:Coyoneda) in `Data.Functor.Coyoneda`, and `fmap'` is identical to [`lowerCoyoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Coyoneda.html#v:lowerCoyoneda). With `lowerCoyoneda` and [`liftCoyoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Coyoneda.html#v:liftCoyoneda), you can say that `SomeFA f` and `f` are naturally isomorphic.

Next, let's flip parameters of `fmap`. You'll get `Functor f => f a -> (a -> b) -> f b`. `b` only appears in the return value `(a -> b) -> f b`, so it should be universal. Let's define this universal type.

```
type AnyFA :: (Type -> Type) -> Type -> Type
newtype AnyFA f a = MkAnyFA (forall x. (a -> x) -> f x)
```

Again, you can write `fmap''` like this,

```
fmap'' :: Functor f => f a -> AnyFA f a
fmap'' fa = MkAnyFA (flip fmap fa)
```

`fmap` and `fmap''` are isomorphic, and `fmap''` is a natural transformation. You can write its type as `Functor f => f ~> AnyFA f`. This means that there should be a natural transformation from `f` to `AnyFA f` if `f` is `Functor`.

This `AnyFA` is identical to [`Yoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Yoneda.html#t:Yoneda) in `Data.Functor.Yoneda`, and `fmap''` is identical to [`liftYoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Yoneda.html#v:liftYoneda). With `liftYoneda` and [`lowerYoneda`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Yoneda.html#v:lowerYoneda), you can say that `AnyFA f` and `f` are naturally isomorphic.

Let's take another example. This time we use `liftA2`. Its type is `Applicative f => (a -> b -> c) -> f a -> f b -> f c`. When you uncurry it, you'll get `Applicative f => (a -> b -> c, f a, f b) -> f c`. With this `SomeFA2`,

```
type SomeFA2 :: (Type -> Type) -> Type -> Type
data SomeFA2 f a = forall x y. MkSomeFA2 (x -> y -> a) (f x) (f y)
```

you can write `liftA2` like this.

```
liftA2' :: Applicative f => SomeFA2 f a -> f a
liftA2' (MkSomeFA2 g fx fy) = liftA2 g fx fy
```

`SomeFA2` is identical to [`Day`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Day.html#t:Day) in `Data.Functor.Day`, and `liftA2'` is identical to [`dap`](https://hackage.haskell.org/package/kan-extensions-5.2.6/docs/Data-Functor-Day.html#v:dap). Since `x` and `y` in `SomeFA2` are existential, you cannot get them out from it, and you need to evaluate `f x` and `f y` to get `f a`. We can say that `SomeFA2` (and `Day`) forces us to evaluate `f a` and `f b` by making them existential.
