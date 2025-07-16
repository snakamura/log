# A parameter type is existential, a return type is universal, part 2

We saw an existential type `Some`, `SomeC` and `SomeF` in [the previous post](./existential_universal1.html). Now, let's take a look at the opposite, a universal type. A universal type is a type that can be interpreted as any type. For example, when you have a value of type `a` that satisfies `Num a => a` and you can use it as any type that satisfies `Num a => a` such as `Int` or `Double`, it's universal.

You can have such type in Haskell with `RankNTypes` (or an old `PolymorphicComponents`) extension.

```
type Any :: Type
newtype Any = MkAny (forall a. a)
```

Contrary to `Some`, you can get anything from it, but unfortunately you can put nothing to it.

```
intFromAny :: Any -> Int
intFromAny (MkAny a) = a

doubleFromAny :: Any -> Double
doubleFromAny (MkAny a) = a
```

Just like `Some`, we usually use it with some constraints.

```
type AnyC :: (Type -> Constraint) -> Type
newtype AnyC c = MkAnyC (forall a. (c a) => a)
```

This type allows you to put a polymorphic type that implements the constraint to it.

```
intToAny :: Int -> AnyC Num
intToAny n = MkAnyC (fromInteger (toInteger n))
```

Note that what you put isn't the `Int` you passed, but something polymorphic created from the `Int`. So you can get `Double` from it.

```
doubleFromAny :: AnyC Num -> Double
doubleFromAny (MkAnyC a) = a
```

When you put `Int` with another constraint, it'll behave differently.

```
intToAny' :: Int -> AnyC Read
intToAny' n = MkAnyC (read (show n))

doubleFromAny' :: AnyC Read -> Double
doubleFromAny' (MkAnyC a) = a
```

`doubleFromAny (intToAny 1)` converts `1` to `Integer` then converts it to `Double` using `fromInteger` of `Num`. On the other hand, `doubleFromAny' (intToAny' 1)` converts `1` to `String` then converts it to `Double` using `read` via `Read`.

Now when you have `toAnyCRead` like this,

```
toAnyCRead :: String -> AnyC Read
toAnyCRead s = MkAnyC (read s)
```

what's the difference between `toAnyCRead` and this `read'`?

```
read' :: Read a => String -> a
read' = read
```

Just like we did with `Some`, it turned out that they're isomorphic. You can define functions to convert them back and forth.

```
forward :: (forall x. c x => a -> x) -> (a -> AnyC c)
forward g = \a -> MkAnyC (g a)

backward :: (a -> AnyC c) -> (forall x. c x => a -> x)
backward h = \a -> let MkAnyC x = h a in x
```

Notice that `backward . forword == id` and `forward . backward == id`.

In this sense, when we see a generic parameter type `a` that only appears in a return value, you can think it's universal. It roughly means that you have to make it return anything that satisfies its constraint.

Let's extend this a bit more by putting it in a container like we did with `Some`.

```
type AnyF :: (k -> Type) -> Type
newtype AnyF f = MkAnyF (forall a. f a)
```

You can still get anything in a container from it. For example, you can get `[Int]` from it and return its length or even return the first value if it's available.

```
fromAnyF :: AnyF [] -> Int
fromAnyF (MkAnyF l) = let intList :: [Int] = l in length intList

fromAnyF' :: AnyF [] -> Int
fromAnyF' (MkAnyF l) = let intList :: [Int] = l in fromMaybe 0 $ listToMaybe intList
```

Unlike `Any`, you can create `AnyF` by passing a container which is polymorphic over `a`. For example, you can put an empty list or `Nothing`.

```
toAnyF :: Int -> AnyF []
toAnyF _ = MkAnyF []

toAnyF' :: Int -> AnyF Maybe
toAnyF' _ = MkAnyF Nothing
```

This time, `toAnyF` is isomorphic to `const [] :: forall x. x -> [a]`. We can see this by `forward` and `backward`.

```
forward :: (forall x. a -> f x) -> (a -> AnyF f)
forward g = \a -> MkAnyF (g a)

backward :: (a -> AnyF f) -> (forall x. a -> f x)
backward h = \a -> let MkAnyF fx = h a in fx
```

Again, `backward . forward == id` and `forward . backward == id`. You'll find that `toAnyF` is `forward (const [])`.

With a constant functor we used in the previous post, we can make the left hand side of `forward` and the right hand side of `backward` natural transformations.

```
newtype Const a x = MkConst a

type f ~> g = forall x. f x -> g x

forward' :: (Const a ~> f) -> (a -> AnyF f)
forward' g = \a -> MkAnyF (g (MkConst a))

backward' :: (a -> AnyF f) -> (Const a ~> f)
backward' h = \(MkConst a) -> let MkAnyF fx = h a in fx
```

Just like `SomeF` and `Const`, `Const` and `AnyF` form an adjunction. The left adjoint `Const` is a functor from `Hask` to a category of functors where objects are functors and morphisms are natural transformations, and the right adjoint `AnyF` is a functor from a category of functors to `Hask`.

Since `Any` is isomorphic to `AnyF Identity`, you can say `Const a ~> Identity` and `a -> Any` are isomorphic, which means `forall x. a -> x` and `a -> Any` are isomorphic.

Intuitively, you can think this isomorphism this way. A function that returns `Any` cannot put anything to it. Similarly, a function that returns any `x` cannot create it because it must be any type. In both cases, a caller should be able to get anything from a return value. They're isomorphic in this sense.
