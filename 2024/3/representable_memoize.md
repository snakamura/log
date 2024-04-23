# Memoize with `Representable`

It's well-known that a naive implementation of a function to calculate a fibonacci number is slow.

```
{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

import Data.Distributive
import Data.Functor.Rep
import Numeric.Natural

fibRec :: Natural -> Integer
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n - 1) + fibRec (n - 2)
```

This `fibRec` takes a few seconds to calculate the 34th fibonacci number in my local environment.

To improve its performance, let's cache it in a stream of numbers. First, we need `Stream` type that contains an infinite number of values.

```
data Stream a = Stream a (Stream a) deriving (Show, Functor)

atStream :: Stream a -> Natural -> a
atStream (Stream a _) 0 = a
atStream (Stream _ as) n = atStream as (n - 1)
```

`atStream` is a function to get a value at a specified index.

Now, we'll introduce two functions. The first function `toStream` is a function that converts a function `Natural -> a` to `Stream a`.

```
toStream :: (Natural -> a) -> Stream a
toStream f = fmap f naturals
  where
    naturals = Stream 0 (fmap (+1) naturals)
```

When you call this function, you'll get a `Stream a` backed by your function. When you try to get a value at a specific index, it'll call your function and return it. But at the same time, it'll cache the value in itself. It'll return the cached value when you get a value with the same index later

The second function `fromStream` does the opposite. It converts a `Stream a` to `Natural -> a`.

```
fromStream :: Stream a -> (Natural -> a)
fromStream = atStream
```

When you call this function, you'll get a function `Natural -> a`, that returns a value at a specified index in your `Stream a`.

You can convert a function and a stream back and forth now with `toStream` and `fromStream`.

Let's speed up `fibRec` with `toStream`.

```
fibs :: Stream Integer
fibs = toStream fibRec
```

It'll still take a few seconds when you call ``fibs `atStream` 34`` first time. But it'll return a value immediately if you call it one more time. You can even convert back to a function with `fromStream`.

```
fibWithStream :: Natural -> Integer
fibWithStream = fromStream fibs
```

This is great, but it's still slow at the first time. What can we do? The problem is that when you call `fibWithStream 34`, it still use `fibRec` to calculate each fibonacci number. It calculates a fibonacci number for 33 once, 32 twice, 31 three times, 30 five times and so on.

If we can use the stream of fibonacci numbers while calculating these numbers, we should be able to speed it up. So first, let's modify `fibRec` a bit by making it take a function to calculate previous fibonacci numbers.

```
fibF :: (Natural -> Integer) -> (Natural -> Integer)
fibF _ 0 = 0
fibF _ 1 = 1
fibF f n = f (n - 1) + f (n - 2)
```

You need to pass some function as `f` to calculate fibonacci numbers. For example, when you pass `undefined`, this function can calculate fibonacci numbers only for 0 and 1. But when you pass `fibF undefined` as `f`, it can calculate fibonacci numbers for 0, 1 and 2. You can repeat it any number of times you want.

```
fib1, fib2, fib3 :: Natural -> Integer
fib1 = fibF undefined -- Works up to 1
fib2 = fibF (fibF undefined) -- Works up to 2
fib3 = fibF (fibF (fibF undefined)) -- Works up to 3
-- fibN = fibF fibN_1
```

So how can you define `fibN`? It's tricky, but you can define a function that apply a function infinite number of times.

```
fixFun :: ((Natural -> Integer) -> (Natural -> Integer)) -> (Natural -> Integer)
fixFun f = f (fixFun f) -- f (f (f (f (f (f ...)))))
```

It's like you can have an infinite list of numbers in Haskell. The infinite list doesn't actually contain an infinite number of numbers in it, but it'll calculate a value when you try to get a value at a specific index. The same goes for `fixFun`. It can apply the function an infinite number of times, but it'll stop applying it when you get a result. It's like `fib3` won't reach `undefined` when you call `fib3 3`.

So now, you can define `fibFix` with `fibF` and `fixFun`.

```
fibFix :: Natural -> Integer
fibFix = fixFun fibF
```

Note that it's still slow because it's identical to `fibRec`. But now, we can insert a function between each step of the calculation. Let's insert `toStream` and `fromStream`. As we saw, inserting a combination of these functions will cache numbers.

```
fib1', fib2', fib3' :: Natural -> Integer
fib1' = fibF undefined -- Works up to 1
fib2' = fibF (fromStream (toStream (fibF undefined))) -- Works up to 2
fib3' = fibF (fromStream (toStream (fibF (fromStream (toStream (fibF undefined)))))) -- Works up to 3
-- fibN' = fibF (fromStream . toStream . fibN_1')
```

When you call `fibN' n`, it'll call `fibN_1' (n - 1)` and `fibN_1' (n - 2)` through `toStream` and `fromStream`. So `fromStream . toStream . fibN_1'` will cache fibonacci numbers for `n - 1` and `n - 2`. The same goes for `fibN_m'`. It'll cache fibonacci numbers for `n - m` and `n - m - 1`. When `fibN_1'` tries to get a fibonacci number for `n - 2`, it should've already cached by `fromStream . toStream . fibN_2'`. So it can just pick a value from the cache.

You can use `fixFun` again to build the final function `fib`.

```
fib :: Natural -> Integer
fib = fixFun (fromStream . toStream . fibF)
```

This function is much faster than `fibRec` and `fibWithStream` because it caches fibonacci numbers at each layer.

A functor like `Stream` which can be represented by a function is called a representable functor and expressed by [`Representable`](https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Rep.html#t:Representable) type class. `Stream` is an instance of it.

```
instance Representable Stream where
    type Rep Stream = Natural
    tabulate = toStream
    index = fromStream

instance Distributive Stream where
    distribute = distributeRep
```

Note that I added an instance of `Distributive` because `Representable` needs it, but you can ignore it for now.

So `tabulate` is a function to convert a function to a functor like `toStream`, and `index` is a function to convert it back to a function like `fromStream`.

You can make your functor an instance of `Representable` if your functor contains a fixed number of values or an infinite number of values. For instance, you can make `Identity` an instance of `Representable`. Since `Identity` is already an instance of `Representable`, I'll use `Id` instead which is identical to `Identity`.

```
data Id a = Id a deriving Functor

instance Representable Id where
    type Rep Id = ()
    tabulate f = Id (f ())
    index (Id a) () = a

instance Distributive Id where
    distribute = distributeRep
```

In this case, the argument of the function `Rep Id` is `()` because `Identity` can only have one value which can be identified by one value `()`.

When your functor `Pair` have two values, for example, `Rep Pair` will be `Bool` because it can return one value for `True` and the other value for `False`.

```
data Pair a = Pair a a deriving Functor

instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x y) True = x
    index (Pair x y) False = y

instance Distributive Pair where
    distribute = distributeRep
```

Of course, you can use a functor other than `Stream` for a function `Natural -> a`. For example, you can use a tree instead.

But you cannot make a list (`[]`) or `Maybe` an instance of `Representable` because it can contain different numbers of values.

Anyway, with `Representable`, you can factor our a function to memoize values from `fib`. First, let's make `fixFun` a bit more generic.

```
fix :: (a -> a) -> a
fix f = f (fix f)
```

Now, you can apply `fix` to any functions other than `((Natural -> Integer) -> (Natural -> Integer)) -> (Natural -> Integer)`. Using it, let's factor out a function from `fib` next. You can use this `memoize` function to cache values of a function who has any corresponding representable functor.

```
memoize :: forall f a. Representable f => ((Rep f -> a) -> (Rep f -> a)) -> (Rep f -> a)
memoize g = fix (index . tabulate @f . g)
```

As `index . tabulate` can use any intermediate functor, you need to specify which functor it should use by specifying `@f`.

Finally, you can define `fib'` in terms of `memoize` now. Again, you need to specify which functor you want to use. This time, we use `Stream` by passing `@Stream`.

```
fib' :: Natural -> Integer
fib' = memoize @Stream fibF
```
