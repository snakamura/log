# `Writer` monad and `Traced` comonad

Next, let's see how [Writer monad](http://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer.html) and [Traced comonad](http://hackage.haskell.org/package/comonad/docs/Control-Comonad-Traced.html) look similar (or not similar).

With `Writer`, you can `tell` a message and compute a value.

```
import Control.Monad
    ( (>>=)
    , (>=>)
    )
import Control.Monad.Writer
    ( Writer
    , tell
    , runWriter
    )
import Data.Functor (($>))

add :: Int -> Writer String Int
add v = tell ("Adding 10 to " ++ show v ++ "\n") $> v + 10

mul :: Int -> Writer String Int
mul v = tell ("Multiplying " ++ show v ++ " by 2\n") $> v * 2

result, result' :: (Int, String)
result = runWriter (add 5 >>= mul)
result' = runWriter ((add >=> mul) 5)
```

With `Traced`, you can `trace` a message and compute a value. It doesn't look it makes much sense to use `trace` to emit a message because you can access the entire object (`Object` in this case). But each function (`add` and `mul`) doesn't need to know how a message will be stored in it.

Note that mesages will be stored in a reverse order compared to `Writer`.

```
import Control.Comonad
    ( (=>>)
    , (=>=)
    , extract
    )
import Control.Comonad.Traced
    ( Traced
    , trace
    , traced
    )

data Object = O
    { value :: Int
    , message :: String
    } deriving Show

add :: Traced String Object -> Object
add t = extract $ t =>>
            trace ("Adding 10 to " ++ show (value $ extract t) ++ "\n") =>>
            \t -> let o = extract t in o { value = value o + 10 }

mul :: Traced String Object -> Object
mul t = extract $ t =>>
            trace ("Multiplying " ++ show (value $ extract t) ++ " by 2\n") =>>
            \t -> let o = extract t in o { value = value o * 2 }

result, result' :: Object
result = extract $ (traced $ \m -> O 5 m) =>> add =>> mul
result' = (add =>= mul) $ traced $ \m -> O 5 m
```

Here are simple implementations of `Writer` monad and `Traced` comonad.

```
{-# LANGUAGE InstanceSigs #-}

import Control.Monad ((>=>))
import Data.Functor (($>))

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f (Writer (x, w)) = Writer (f x, w)

instance Monoid w => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    () :: Writer w (a -> b) -> Writer w a -> Writer w b
    Writer (f, fw)  Writer (x, xw) = Writer (f x, fw  xw)

instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    Writer (x, xw) >>= f = let Writer (y, yw) = f x in Writer (y, xw  yw)

tell :: w -> Writer w ()
tell x = Writer ((), x)
```

```
{-# LANGUAGE InstanceSigs #-}

import Control.Comonad
    ( Comonad
    , (=>>)
    , (=>=)
    , extract
    , extend
    )

newtype Traced w a = Traced (w -> a)

instance Functor (Traced w) where
    fmap f (Traced g) = Traced $ f. g

instance Monoid w => Comonad (Traced w) where
    extract :: Traced w a -> a
    extract (Traced f) = f mempty
    extend :: (Traced w a -> b) -> Traced w a -> Traced w b
    extend f (Traced g) = Traced $ \xw -> f $ Traced (\yw -> g $ xw  yw)

trace :: w -> Traced w a -> a
trace x (Traced f) = f x
```
