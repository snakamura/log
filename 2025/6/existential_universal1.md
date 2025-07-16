# A parameter type is existential, a return type is universal, part 1

You can think an existential type as a type that exists but it's actual type is unknown. For example, when you have a value of type `a` that satisfies `Num a => a` but you cannot know whether `a` is `Int` or `Double` or anything, it's existential.

In Haskell, you can have such a type with `ExistentialQuantification` extension.

```
type Some :: Type
data Some = forall a. MkSome a
```

You can put any value into it to get `Some`. For example, `MkSome True` is `Some`, `MkSome (1 :: Int)` is `Some`, and `MkSome (Just 'a')` is `Some`.

On the other hand, you can do little when you get a value out of it. When you pattern match on `MkSome a`, `a` can be any type and there is nothing you can do with it other than applying `id`.

```
fromSome :: Some -> Some
fromSome (MkSome a) = MkSome (id a)
```

Usually, we use it with some constraints.

```
type SomeC :: (Type -> Constraint) -> Type
data SomeC c = forall a. (c a) => MkSomeC a
```

With a constraint, you can put only a value that implements a constraint `c` to `SomeC` such as `MkSomeC True :: SomeC Show`. You can do more than just applying `id` with it using the constraint `Show a` when you pattern match on `MkSomeC a`.

```
fromSomeCShow :: SomeC Show -> String
fromSomeCShow (MkSomeC a) = show a
```

Then, what's the difference between `fromSomeCShow` and this `show'`?

```
show' :: Show a => a -> String
show' = show
```

Their types are different, but are there anything we can do with one of them but we cannot do with the other? It turned out that they're isomorphic. You can define two functions converting them back and forth.

```
forward :: (SomeC c -> a) -> (forall x. c x => x -> a)
forward g = \x -> g (MkSomeC x)

backward :: (forall x. c x => x -> a) -> (SomeC c -> a)
backward h = \(MkSomeC x) -> h x
```

You'll notice that `backward . forword == id` and `forward . backward == id`.

In this sense, you can think it's existential when you see a generic parameter type `a` that only appears in a parameter. It roughly means that you can only do what you can do with its constraint.

Let's extend this a bit more. We added a constraint above to make it usable. Another way of making it usable is to put it in a container.

```
type SomeF :: (k -> Type) -> Type
data SomeF f = forall a. MkSomeF (f a)
```

You can create it by putting anything into it like we saw above.

```
toSomeF :: Char -> SomeF []
toSomeF c = MkSomeF [c]
```

This time, we can get something out of it, not just applying `id`. For example, we can get length of the list.

```
fromSomeF :: SomeF [] -> Int
fromSomeF (MkSomeF l) = length l
```

As you see, this works no matter what `a` is because the type of `length` is `forall a. [a] -> Int`, which means it works with any `a`. You can say that `fromSomeF` and `length` are isomorphic. Let's define two functions converting them back and forth.

```
forward :: (SomeF f -> a) -> (forall x. f x -> a)
forward g = \fx -> g (MkSomeF fx)

backward :: (forall x. f x -> a) -> (SomeF f -> a)
backward h = \(MkSomeF fx) -> h fx
```

Again, `backward . forward == id` and `forward . backward == id`.

You will find that `fromSomeF` is `backward length`.

Now, let's dig this a bit more. First, let's define a constant functor. It's equivalent to `Data.Functor.Const`.

```
newtype Const a x = MkConst a
```

Then, you can make `forward` and `backward` use `Const` using the fact that `a` is isomorphic to `Const x a` for any `x`.

```
forward' :: (SomeF f -> a) -> (forall x. f x -> Const a x)
forward' g = \fx -> MkConst (g (MkSomeF fx))

backward' :: (forall x. f x -> Const a x) -> (SomeF f -> a)
backward' h = \(MkSomeF fx) -> let MkConst a = h fx in a
```

You'll find that the right hand side of `forward'` and the left hand side of `backward'` are natural transformations because they're polymorphic over any `x`. When you introduce `~>` to express a natural transformation,

```
type f ~> g = forall x. f x -> g x
```

you can write `forward'` and `backward'` like these.

```
forward'' :: (SomeF f -> a) -> (f ~> Const a)
forward'' g = \fx -> MkConst (g (MkSomeF fx))

backward'' :: (f ~> Const a) -> (SomeF f -> a)
backward'' h = \(MkSomeF fx) -> let MkConst a = h fx in a
```

Don't they look similar to `leftAdjunction` and `rightAdjunction` of `Adjunction` we saw in [this post](../../2024/2/products_functions.html)?

```
class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a)
  unit = leftAdjunct id

  counit :: f (g a) -> a
  counit = rightAdjunct id

  leftAdjunct :: (f a -> b) -> (a -> g b)
  leftAdjunct f = fmap f . unit

  rightAdjunct :: (a -> g b) -> (f a -> b)
  rightAdjunct f = counit . fmap f
```

It turned out that `SomeF` is a left adjoint and `Const` is a right adjoint. Unfortunately, we cannot make them an instance of this `Adjunction`. `Adjunction` defines an adjunction whose left adjoint and right adjoint are both endofunctors in the category of Haskell types `Hask` where objects are types and morphisms are functions. But in an adjunction of `SomeF` and `Const`, the left adjoint `SomeF` is a functor from a category of functors, where objects are functors and morphisms are natural transformations, to `Hask`, and the right adjoint `Const` is a functor from `Hask` to a category of functors. Still, they are an adjunction.

Since `Some` is isomorphic to `SomeF Identity`, you can say `Some -> a` and `Identity ~> Const a` are isomorphic, which means `Some -> a` and `forall x. x -> a` are isomorphic.

Intuitively, you'd think this isomorphism this way. A function that takes `Some` cannot get anything from it because it's unknown what's inside it. Similarly, a function that takes any `x` cannot get anything from `x` because it can be anything. In both cases, a caller can pass anything to them as their argument. They're isomorphic in this sense.
