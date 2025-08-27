# Pairs and functions, part 1

As we saw in [Products and functions](../2024/2/products_functions.html), there is an adjunction between product and function. You can define `leftAdjunct` and `rightAdjunct` like these.

```
leftAdjunct :: ((,) e a -> b) -> (a -> (->) e b)
leftAdjunct f a e = f (e, a)

rightAdjunct :: (a -> (->) e b) -> ((,) e a -> b)
rightAdjunct f (e, a) = f a e
```

`leftAdjunct` is isomorphic to `curry`, and `rightAdjunct` is isomorphic to `uncurry`. This means that `(,) e a -> b` (`(e, a) -> b`) and `a -> (->) e b` (`a -> e -> b`) are isomorphic.

When you think that a function `e -> a` is a map from `e` to `a`, you can think them this way. Passing `e` and `a` to a function to get `b` is identical to passing `a` to a function to get a map from `e` to `b`. As we saw in [A parameter type is existential, a return type is universal, part 1](../2025/6/existential_universal1.html), `e` is existential in the former because `e` appears as a parameter, and is universal in the latter because `e` appears as a return type.

Now, let's see how we can compose functions of type `a -> (->) e b`. For example, we want to compose `a -> (->) e b` and `b -> (->) e c` to get `a -> (->) e c`. As you've noticed, it's `Reader` monad.

```
newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
  fmap :: (a -> b) -> Reader e a -> Reader e b
  fmap a2b (Reader e2a) = Reader $ a2b . e2a

instance Applicative (Reader e) where
  pure :: a -> Reader e a
  pure a = Reader $ const a

  (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
  Reader e2a2b <*> Reader e2a = Reader $ \e -> e2a2b e (e2a e)

instance Monad (Reader e) where
  (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  Reader e2a >>= a2rb = Reader $ \e -> let Reader e2b = a2rb (e2a e) in e2b e
```

You can define functions `r1`, `r2` and `r3` and compose them like this.

```
withReader :: Int
withReader =
  let r1 :: Int -> Reader Int String
      r1 n = Reader $ show . (+ (n + 1))
      r2 :: String -> Reader Int Int
      r2 s = Reader (* (length s + 2))
      r3 :: Int -> Reader Int Int
      r3 n = Reader $ \e -> let Reader e'2a = Reader $ \e' -> e' + n in e'2a (e * 2)

      initialValue = 100
      Reader e2a = pure initialValue >>= r1 >>= r2 >>= r3
      env = 10
   in e2a env
```

With helper functions `ask` and `local`, you can make it look more like code using a reader monad.

```
ask :: Reader e e
ask = Reader id

local :: (e -> e) -> Reader e a -> Reader e a
local e2e (Reader e2a) = Reader $ e2a . e2e
```

```
withReader' :: Int
withReader' =
  let r1 :: Int -> Reader Int String
      r1 n = ask >>= \m -> pure $ show (m + n + 1)
      r2 :: String -> Reader Int Int
      r2 s = ask >>= \m -> pure $ m * (length s + 2)
      r3 :: Int -> Reader Int Int
      r3 n = local (* 2) $ ask >>= \e -> pure $ e + n

      initialValue = 100
      Reader e2a = pure initialValue >>= r1 >>= r2 >>= r3
      env = 10
   in e2a env
```

Then, how can we compose functions of type `(,) e a -> b`? For example, we want to compose `(,) e a -> b` and `(,) e b -> c` to get `(,) e a -> c`. It's `Env` comonad.

```
newtype Env v a = Env (v, a)

instance Functor (Env v) where
  fmap :: (a -> b) -> Env v a -> Env v b
  fmap a2b (Env (v, a)) = Env (v, a2b a)

instance Comonad (Env v) where
  extract :: Env v a -> a
  extract (Env (_, a)) = a

  extend :: (Env v a -> b) -> Env v a -> Env v b
  extend ea2b ea@(Env (v, _)) = Env (v, ea2b ea)

  duplicate :: Env v a -> Env v (Env v a)
  duplicate (Env (v, a)) = Env (v, Env (v, a))
```

You can define functions `e1`, `e2` and `e3` and compose them.

```
withEnv :: Int
withEnv =
  let e1 :: Env Int Int -> String
      e1 (Env (v, n)) = show $ v + n + 1
      e2 :: Env Int String -> Int
      e2 (Env (v, s)) = v * (length s + 2)
      e3 :: Env Int Int -> Int
      e3 (Env (v, n)) = let e3' (Env (v', n')) = v' + n' in e3' (Env (v * 2, n))

      env = 10
      initialValue = 100
      Env (_, a) = Env (env, initialValue) =>> e1 =>> e2 =>> e3
   in a
```

Once you've defined `ask` and `local` for this comonad, you can write these functions without using `Env` data constructor directly.

```
ask :: Env v a -> v
ask (Env (v, _)) = v

local :: (v -> v) -> Env v a -> Env v a
local v2v (Env (v, a)) = Env (v2v v, a)
```

```
withEnv' :: Int
withEnv' =
  let e1 :: Env Int Int -> String
      e1 e = show $ ask e + extract e + 1
      e2 :: Env Int String -> Int
      e2 e = ask e * (length (extract e) + 2)
      e3 :: Env Int Int -> Int
      e3 e = let e3' e' = ask e' + extract e' in e3' $ local (* 2) e

      env = 10
      initialValue = 100
      Env (_, a) = Env (env, initialValue) =>> e1 =>> e2 =>> e3
   in a
```

As you can see, both `Reader` monad and `Env` comonad allow you to get a readonly value in these functions `r<N>` and `e<N>`. This isn't a coincidence, but it comes from the fact that `(,) e a -> b` and `a -> (->) e b` are isomorphic.
