# `Reader` monad and `Env` comonad

It is said that [Env comonad](http://hackage.haskell.org/package/comonad/docs/Control-Comonad-Env.html) is identical to [Reader monad](http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader.html). Then how can you write the same code using `Reader` and `Env`?

Let's take a very simple example that uses `Reader`.

```
import Control.Monad
    ( (>>=)
    , (>=>)
    , return
    )
import Control.Monad.Reader
    ( Reader
    , ask
    , runReader
    )

add :: Float -> Reader Int Float
add n = ask >>= \e -> return $ n + fromIntegral e

mul :: Float -> Reader Int Float
mul n = ask >>= \e -> return $ n * fromIntegral e

result, result' :: Float
result = runReader (add 2 >>= mul) 3
result' = runReader ((add >=> mul) 2) 3
```

How will it look when you write the same code using `Env`?

```
import Control.Comonad
    ( (=>>)
    , (=>=)
    , extract
    )
import Control.Comonad.Env
    ( Env
    , ask
    , env
    )

add :: Env Int Float -> Float
add env = fromIntegral (ask env) + extract env

mul :: Env Int Float -> Float
mul env = fromIntegral (ask env) * extract env

result, result' :: Float
result = extract $ env 3 2 =>> add =>> mul
result' = (add =>= mul) $ env 3 2
```

When you use `Reader`, each function (`add` and `mul`) *returns* a `Reader` to lift a value into `Reader` monad. The bind operator (`>>=`) passes an environment from a functon to another function.

On the other hand, with `Env`, each function *takes* an `Env` and extract a value from `Env` comonad. The extend operator (`=>>`) passes an environment from a function to another function.

It'll help understand what's going on under the hood by implementing `Reader` and `Env` by yourself.

```
{-# LANGUAGE InstanceSigs #-}

import Control.Monad ((>=>))

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
    fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader e) where
    pure x = Reader $ \e -> x
    () :: Reader e (a -> b) -> Reader e a -> Reader e b
    Reader f  Reader x = Reader $ \e -> f e (x e)

instance Monad (Reader e) where
    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    Reader x >>= f = Reader $ \e -> let Reader y = f (x e) in y e

ask :: Reader e e
ask = Reader id
```

`>>=` applies the same environment to the input (`x`) and the output (`y`).

```
{-# LANGUAGE InstanceSigs #-}

import Control.Comonad
    ( Comonad
    , (=>>)
    , (=>=)
    , extract
    , extend
    )

newtype Env e a = Env (e, a)

instance Functor (Env e) where
    fmap f (Env (e, x)) = Env (e, f x)

instance Comonad (Env e) where
    extract :: Env e a -> a
    extract (Env (_, a)) = a
    extend :: (Env e a -> b) -> Env e a -> Env e b
    extend f env@(Env (e, x)) = Env (e, f env)

ask :: Env e a -> e
ask (Env (e, _)) = e
```

`extend` takes out the environment from the input (`e`) and uses it to evaluate the function (`f`), and uses the same environment to create a return value too.
