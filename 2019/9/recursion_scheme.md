# Calculating fibonacci numbers using recursion schemes

After reading [the good articles](https://blog.sumtypeofway.com) about recursion scheme by Patrick Thomson, I tried the example in [Recursion Schemes, Part IV: Time is of the Essence](https://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/) to calculate fibonacci numbers using histomorphism.

First, let's define `histo` function. I just took these definitions from those articles.

```
{-# LANGUAGE DeriveFunctor, StandaloneDeriving, UndecidableInstances #-}

import Control.Arrow ((>>>), ( Show (Term f)

type Coalgebra f a = a -> f a
ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana coalg = In  a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute
  where
    worker = out >>> fmap worker >>> (h &&& id) >>> uncurry Attr
```

Now, you need a number type defined using recursions.

```
data NatF a = ZeroF
            | SuccF a
 deriving (Show, Functor)

type Nat = Term NatF
```

You can generate a number of `Nat` from an ordinal number by using `ana`.

```
nat :: (Eq n, Num n) => n -> Nat
nat n = ana build n
  where
    build 0 = ZeroF
    build n = SuccF (n - 1)
```

Then, what you need to do is to write a function that calculates a fibonacci number from `NatF (Attr NatF a)`.

```
fibF :: Num n => NatF (Attr NatF n) -> n
fibF ZeroF = 0
fibF (SuccF (Attr a ZeroF)) = 1
fibF (SuccF (Attr a (SuccF (Attr b _)))) = a + b
```

As you can see, you pick the previous fibonacci number directly and pick the previous previous fibonacci number from the history in the last pattern.

You can now calculate a fibonacci number by passing `fibF` to `histo`.

```
fib :: (Eq n, Num n, Num m) => n -> m
fib n = histo fibF $ nat n
```

When you compare this `fib` with this naive implementation (`fib'`), you'll find that `fib` runs significantly faster than `fib'` because it caches intermediate fibonacci numbers.

```
fib' :: (Eq n, Num n) => n -> n
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)
```

By the way, you don't need to write most of the code above when you use [recursion-schemes](http://hackage.haskell.org/package/recursion-schemes). As described in [Recursion Schemes, Part 4½: Better Living Through Base Functors](https://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/), it provides a base functor for [`GHC.Natural.Natural`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Natural.html#t:Natural) (it's `Maybe` because our `NatF` is isomorphic to `Maybe`), and lifts `Natural` to `Maybe` automatically. Also it uses [`Control.Comonad.Cofree`](https://hackage.haskell.org/package/free-5.1/docs/Control-Comonad-Cofree.html#t:Cofree) instead of our `Attr`.

So what you need to do is just write `fibF` for it.

```
{-# LANGUAGE TypeApplications #-}

import Control.Comonad.Cofree (Cofree((:<)))
import Data.Functor.Foldable (Base, histo)
import GHC.Natural (Natural)

fibF :: Num n => (Base Natural) (Cofree (Base Natural) n) -> n
fibF Nothing = 0
fibF (Just (a :< Nothing)) = 1
fibF (Just (a :< Just (b :< _))) = a + b

fib :: (Integral n, Num m) => n -> m
fib n = histo fibF $ fromIntegral @_ @Natural n
```
