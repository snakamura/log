# Implementing an instance of a type class dynamically, part 3

We defined `BindY` and `bindY'` in [the previous post](./reflection2.html). They handled only `DictY` which is a dictionary for constraint `Y`, but we can generalize them to any constraints.

```
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.Exts (Any, withDict)

type Bind :: k -> Type -> Constraint
class Bind s a | s -> a where
  get :: Proxy s -> a

bind :: forall a r. a -> (forall k (s :: k). (Bind s a) => Proxy s -> r) -> r
bind a f =
  withDict
    @(Bind (Any @Any) a)
    (const a)
    (f @Any @Any)
    Proxy

type Wrap :: k -> Type -> Type
newtype Wrap s a = Wrap a
```

Now you can use `bind` with any type classes other than `Y`.

Since `get` now returns `a` instead of `DictY a`, we need `Bind s (DictY a)` constraint instead of `BindY s a` when we implement `Y` for `Wrap s a`. Note that we need `UndecidableInstances` extension to do this.

```
class Y a where
  y1 :: a -> String
  y2 :: a -> a -> Int

instance Y String where
  y1 = id
  y2 _ _ = 0

instance Y Int where
  y1 = show
  y2 = (+)

type DictY :: Type -> Type
data DictY a = DictY
  { _y1 :: a -> String,
    _y2 :: a -> a -> Int
  }

instance (Bind s (DictY a)) => Y (Wrap s a) where
  y1 (Wrap a) = _y1 (get (Proxy :: Proxy s)) a
  y2 (Wrap a1) (Wrap a2) = _y2 (get (Proxy :: Proxy s)) a1 a2

v1 :: String
v1 = bind
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y1 (Wrap 10 :: Wrap s Int)

v2 :: Int
v2 = bind
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y2 (Wrap 10 :: Wrap s Int) (Wrap 20 :: Wrap s Int)
```

This machinery is implemented in [`reflection` package](https://hackage.haskell.org/package/reflection). `Bind` is called `Reifies`, `bind` is called `reify` and `get` is called `reflect` in it. This is because `reify` reifies a value (a type class dictionary in our case) as a type `s`, and `reflect` allows you to get the original value from `s`.
