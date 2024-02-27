# Expressing type equality

```
{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             TypeApplications,
             TypeOperators
#-}

import Data.Type.Equality
```

Imagine that you have a function `foo` taking two arguments of the same type.

```
foo :: a -> a -> ()
foo _ _ = ()
```

And you want to call it from another function somehow. The simplest way is of course this.

```
bar1 :: a -> a -> ()
bar1 = foo
```

You can use `~` to express type equality. This is almost identical to `bar1`.

```
bar2 :: a ~ b => a -> b -> ()
bar2 = foo
```

Another way of expressing the equality is passing the witness as a value. We can pass a value of [`(:~:)`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Type-Equality.html#t::-126-:). When a caller passes `Refl`, the type checker is convinced that `a` and `b` are the same type.

```
bar3 :: a :~: b -> a -> b -> ()
bar3 Refl = foo
```

You can declare a type class to pass this witness implicitly, but it might be even cumbersome to declare its instances.

```
class E a b where
    e :: a :~: b

bar4 :: forall a b. E a b => a -> b -> ()
bar4 x y = case e @a @b of
               Refl -> foo x y
```

Use [`TestEquality`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Type-Equality.html#t:TestEquality) to check the equality at runtime. You'd use this with singleton types.

```
bar5 :: TestEquality f => f a -> f b -> ()
bar5 x y = case testEquality x y of
               Just Refl -> foo x y
               Nothing -> ()
```
