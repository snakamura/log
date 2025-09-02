# Pairs and functions, part 2

We saw how we could compose `a -> (->) e b` and `(,) e a -> b` in [the previous post](./pair_function1.html). It turned out that they were `Reader` monad and `Env` comonad.

Now, let's flip the functors and try to compose `a -> (,) e b` and `(->) e a -> b`.

Let's start with `a -> (,) e b`, and compose `a -> (,) e b` and `b -> (,) e c`. You might've noticed, it's `Writer` monad.

```
newtype Writer m a = Writer (m, a)

instance Functor (Writer m) where
  fmap :: (a -> b) -> Writer m a -> Writer m b
  fmap a2b (Writer (m, a)) = Writer (m, a2b a)

instance (Monoid m) => Applicative (Writer m) where
  pure :: a -> Writer m a
  pure a = Writer (mempty, a)

  (<*>) :: Writer m (a -> b) -> Writer m a -> Writer m b
  Writer (ma2b, a2b) <*> Writer (ma, a) = Writer (ma2b <> ma, a2b a)

instance (Monoid m) => Monad (Writer m) where
  (>>=) :: Writer m a -> (a -> Writer m b) -> Writer m b
  Writer (ma, a) >>= a2wb = let Writer (mb, b) = a2wb a in Writer (ma <> mb, b)
```

I used `m` instead of `e` here to make it clear that it's `Monoid`. You can define `w1`, `w2`, and `w3` and compose them.

```
withWriter :: (String, Int)
withWriter =
  let w1 :: Int -> Writer String String
      w1 n = Writer ("1st\n", show $ n + 1)
      w2 :: String -> Writer String Int
      w2 s = Writer ("2nd\n", length s * 10)
      w3 :: Int -> Writer String Int
      w3 = pure

      initialValue = 100
      Writer (w, a) = return initialValue >>= w1 >>= w2 >>= w3
   in (w, a)
```

Again, with help of utility function `tell`, you can make it look more monad-like.

```
tell :: m -> Writer m ()
tell m = Writer (m, ())
```

```
withWriter' :: (String, Int)
withWriter' =
  let w1 :: Int -> Writer String String
      w1 n = tell "1st\n" >> pure (show $ n + 1)
      w2 :: String -> Writer String Int
      w2 s = tell "2nd\n" >> pure (length s * 10)
      w3 :: Int -> Writer String Int
      w3 = pure

      initialValue = 100
      Writer (w, a) = return initialValue >>= w1 >>= w2 >>= w3
   in (w, a)
```

When you compose `a -> (,) e b`, you need to combine `e` somehow without knowing its actual type. `Writer` combines them using `(<>)` which adds `Monoid` constraint to `e`.

Can we have the same functionality by composing `(->) e a -> b` then just like `Reader` monad and `Env` comonad had the same functionality? It turns out it doesn't. Let's see what we get by composing `(->) e a -> b` and `(->) e b -> c`. It's `Traced` comonad.

```
newtype Traced m a = Traced (m -> a)

instance Functor (Traced m) where
  fmap :: (a -> b) -> Traced m a -> Traced m b
  fmap a2b (Traced m2a) = Traced (a2b . m2a)

instance (Monoid m) => Comonad (Traced m) where
  extract :: (Traced m a) -> a
  extract (Traced m2a) = m2a mempty

  extend :: (Traced m a -> b) -> Traced m a -> Traced m b
  extend ta2b (Traced m2a) = Traced $ \mb -> ta2b (Traced $ \ma -> m2a (ma <> mb))

  duplicate :: Traced m a -> Traced m (Traced m a)
  duplicate (Traced m2a) = Traced $ \mta -> Traced $ \ma -> m2a (mta <> ma)
```

As we saw in the previous post, `Env` comonad allows you to get a single value. `Traced` comonad extends `Env` comonad by allowing you to get a value indexed by `m`. And, you can move an index by using `Monoid`.

`(->) m a` is a map from `m` to `a`. A function of type `(->) m a -> b` says that "Give me a map from `m` to `a`, then I'll give you `b`. " The function gets `a` from this map using a fixed value as an index. When you compose `(->) m a -> b` and `(->) m b -> c`, the first function passes a new map as well as how much it should shift an index to the second function. The second function gets `b` from the map using its own index shifted by the amount passed from the first function.

```
withTraced :: Double
withTraced =
  let gain :: Double -> Traced (Sum Int) Double -> Double
      gain g (Traced m2a) = g * m2a (Sum 0)
      delay :: Int -> Traced (Sum Int) Double -> Double
      delay d (Traced m2a) = m2a (Sum (-d))
      identity :: Traced (Sum Int) Double -> Double
      identity = extract

      original :: Traced (Sum Int) Double
      original = Traced $ \(Sum n) -> sin (fromIntegral n)
      Traced m2a' = original =>> gain 2 =>> delay 3 =>> identity
   in m2a' mempty
```

In this example, `gain` doesn't shift an index (`Sum 0`), and `delay` shifts it `-d` (`Sum (-d)`).

With a helper function `trace`, you can write it without using `Traced` data constructor directly.

```
trace :: m -> Traced m a -> a
trace m (Traced m2a) = m2a m
```

```
withTraced' :: Double
withTraced' =
  let gain :: Double -> Traced (Sum Int) Double -> Double
      gain g t = g * trace (Sum 0) t
      delay :: Int -> Traced (Sum Int) Double -> Double
      delay d = trace (Sum (-d))
      identity :: Traced (Sum Int) Double -> Double
      identity = extract

      original :: Traced (Sum Int) Double
      original = Traced $ \(Sum n) -> sin (fromIntegral n)
      Traced m2a' = original =>> gain 2 =>> delay 3 =>> identity
   in m2a' mempty
```

While `Writer` concatenates an output `m`, `Traced` concatenates an index `m` of an input map `m -> a`, and they behave completely different ways.
