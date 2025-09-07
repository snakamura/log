# Pairs and functions, part 3

Now, we've looked at how we could compose `a -> (->) e b`, `(,) e a -> b`, `a -> (,) m b)` and `(->) m a -> b`. Now, let's compose functors `(,) e` and `(->) e` themselves and see what happens.

When you compose `(->) t` and `(,) t` , you'll get a new functor `F t a = (->) t ((,) t a)` which is `t -> (t, a)`. When you compose `(,) t` and `(->) t`, you'll get another new functor `G t a = (,) t ((->) t a)` which is `(t, t -> a)`. Now, let's try compose functions returning the first functor (`a -> (t -> (t, b))`), first. Obviously, it's a famous `State` monad.

```
newtype State t a = State (t -> (t, a))

instance Functor (State t) where
  fmap :: (a -> b) -> State t a -> State t b
  fmap a2b (State t2ta) = State $ \t -> let (t', a) = t2ta t in (t', a2b a)

instance Applicative (State t) where
  pure :: a -> State t a
  pure a = State (,a)

  (<*>) :: State t (a -> b) -> State t a -> State t b
  State t2ta2b <*> State t2ta = State $ \t ->
    let (t', a2b) = t2ta2b t
        (t'', a) = t2ta t'
     in (t'', a2b a)

instance Monad (State t) where
  (>>=) :: State t a -> (a -> State t b) -> State t b
  State t2ta >>= a2sb = State $ \t ->
    let (t', a) = t2ta t
        State t2tb = a2sb a
     in t2tb t'
```

`State` monad extends `Reader` monad by allowing you to update its state. While `Writer` monad allows you only to append a monoidal value, `State` monad allows you to put an arbitrary value.

```
withState :: (String, Int)
withState =
  let s1 :: Int -> State String String
      s1 n = State $ \t -> (t <> "!!!", t <> show (n + 10))
      s2 :: String -> State String Int
      s2 s = State $ \t -> (t <> s, length s)
      s3 :: Int -> State String Int
      s3 = pure

      initialValue = 100
      State t2ta = pure initialValue >>= s1 >>= s2 >>= s3
      initialState = "state"
   in t2ta initialState
```

With helper functions `get`, `put` and `modify`, it'll be more monad-like code.

```
get :: State t t
get = State $ \t -> (t, t)

put :: t -> State t ()
put t = State $ \_ -> (t, ())

modify :: (t -> t) -> State t ()
modify f = State $ \t -> (f t, ())
```

```
withState' :: (String, Int)
withState' =
  let s1 :: Int -> State String String
      s1 n = get >>= \t -> put (t <> "!!!") >> pure (t <> show (n + 10))
      s2 :: String -> State String Int
      s2 s = modify (<> s) >> pure (length s)
      s3 :: Int -> State String Int
      s3 = pure

      initialValue = 100
      State t2ta = pure initialValue >>= s1 >>= s2 >>= s3
      initialState = "state"
   in t2ta initialState
```

When you uncurry `a -> (t -> (t, b))`, you'll get `(a, t) -> (t, b)`. Once you've flipped the first pair, you'll get `(t, a) -> (t, b)`. So composing `State` monad is the same thing as composing functions from a state-value pair to a state-value pair.

Then, what happens when you compose functions taking the second functor (`(t, t -> a) -> b`)? It's called `Store` comonad.

```
newtype Store t a = Store (t, t -> a)

instance Functor (Store t) where
  fmap :: (a -> b) -> Store t a -> Store t b
  fmap a2b (Store (t, t2a)) = Store (t, a2b . t2a)

instance Comonad (Store t) where
  extract :: Store t a -> a
  extract (Store (t, t2a)) = t2a t

  extend :: (Store t a -> b) -> Store t a -> Store t b
  extend sa2b (Store (t, t2a)) = Store (t, \t' -> sa2b (Store (t', t2a)))

  duplicate :: Store t a -> Store t (Store t a)
  duplicate (Store (t, t2a)) = Store (t, \t' -> Store (t', t2a))
```

`Store` comonad extends `Traced` comonad by allowing you to peek values at any indices. It still has a current index just like `Traced` comonad though. As we saw in [the previous post](./pair_function2.html), you can shift a current monoidal index and make the following functions use that index as a current index with `Traced` comonad. With `Store` comonad, you can let following functions use any specified index in addition to peeking at values at any indices.

```
withStore :: ([Int], Int)
withStore =
  let s1 :: Store Int String -> (String, String)
      s1 (Store (n, n2s)) = (n2s (n - 1), n2s (n + 1))
      s2 :: Store Int (String, String) -> Int
      s2 (Store (n, n2ss)) = length $ uncurry (<>) $ n2ss n
      s3 :: Store Int Int -> Int
      s3 (Store (n, n2n)) = n2n (n * 2)

      values = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
      store = Store (3, \k -> fromMaybe "" $ lookup k values)
      Store (t', t2a') = store =>> s1 =>> s2 =>> s3
   in (map t2a' [1 .. 5], t2a' t')
```

In this example, `s1` makes a pair of values at the previous and the next index of the current index, and `s2` concatenates those two strings at the current index and gets its length.

We have some helper functions for `Store` comonad as well.

```
pos :: Store t a -> t
pos (Store (t, _)) = t

peek :: t -> Store t a -> a
peek t (Store (_, t2a)) = t2a t

peeks :: (t -> t) -> Store t a -> a
peeks f (Store (t, t2a)) = t2a (f t)

seek :: t -> Store t a -> Store t a
seek t (Store (_, t2a)) = Store (t, t2a)

seeks :: (t -> t) -> Store t a -> Store t a
seeks f (Store (t, t2a)) = Store (f t, t2a)
```

`peek` allows you to get a value at any index. `peeks` is a convenient function to get a value at a specified offset from the current index. `seek` and `seeks` are like `local` for `Reader` and `Env`. They allow you to run your functions with the same map from `t` to `a` but with a modified current index. You can also think that it's like specifying an offset with `Traced` comonad. But, while `Traced` comonad allows you only to specify an offset from the current index, `seek` allows you to specify an arbitrary index.

```
withStore' :: ([Int], Int)
withStore' =
  let s1 :: Store Int String -> (String, String)
      s1 s = (peeks (subtract 1) s, peeks (+ 1) s)
      s2 :: Store Int (String, String) -> Int
      s2 s = length $ uncurry (<>) $ extract s
      s3 :: Store Int Int -> Int
      s3 s = extract $ seeks (* 2) s

      values = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
      store = Store (3, \k -> fromMaybe "" $ lookup k values)
      Store (t', t2a') = store =>> s1 =>> s2 =>> s3
   in (map t2a' [1 .. 5], t2a' t')
```

Even though constructions of `State` monad and `Store` comonad have some similarities, `State` monad and `Store` comonad behave in completely different ways.
