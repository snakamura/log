# Implementing an instance of a type class dynamically, part 2

We successfully used any function as a method of a type class in [the previous post](../4/reflection1.html) if the type class had only one method. In this post, we'll see what we can do with a type class with more than one methods.

First, let's declare a type class with two methods and two instances.

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
```

We usually define a wrapper `newtype` if we want to implement another instance for the same type. For example, we define a `newtype` wrapping a string.

```
newtype WrappedString = WrappedString String

instance Y WrappedString where
  y1 (WrappedString s) = s <> "!!!"
  y2 _ _ = 100
```

But unfortunately, we cannot implement a new `newtype` for all sets of functions. We'll instead define a `newtype` which is indexed by a type. This way, we can define an infinite number of `newtype`s at once.

```
type WrapY :: k -> Type -> Type
newtype WrapY s a = WrapY a
```

As you can see `s` is our index type. Also, we'll define a data type that represents a dictionary of methods of `Y`.

```
type DictY :: Type -> Type
data DictY a = DictY
  { _y1 :: a -> String,
    _y2 :: a -> a -> Int
  }
```

The idea is that we should be able to use functions in `DictY` as its methods if we can associate `WrapY s a` to a value of `DictY`. We know that we can use a type class to associate a value with a type.

```
type BindY :: k -> Type -> Constraint
class BindY s a | s -> a where
  get :: Proxy s -> DictY a
```

This type class allows you to get a method dictionary for `Y` (`DictY a`) from `WrapY s a` (actually only from `s`). If you want to implement an instance of `Y` for `WrapY s a`, you can call `get` to get `DictY a`, and call one of the functions in it.

```
instance (BindY s a) => Y (WrapY s a) where
  y1 (WrapY a) = _y1 (get (Proxy :: Proxy s)) a
  y2 (WrapY a1) (WrapY a2) = _y2 (get (Proxy :: Proxy s)) a1 a2
```

Now we use the trick to use a function as a method of a type class which we used in the previous post again. This time, what we need is using our function as `get`.

```
bindY :: forall s a r. DictY a -> ((BindY s a) => Proxy s -> r) -> r
bindY dictY f =
  withDict
    @(BindY s a)
    @(Proxy s -> DictY a)
    (const dictY)
    f
    (Proxy :: Proxy s)
```

`bindY` takes a dictionary of methods of `Y` for `a` (`dictY`), and a function taking `Proxy s` in CPS style. The type of this function is `(BindY s a) => Proxy s -> r`, but we can cast it to `(Proxy s -> DictY a) -> Proxy s -> r` as we saw in the previous post. Since we already have `DictY a`, we can call this function by passing a function that always returns `dictY` (`const dictY`), and `Proxy s`.

Now, you can use it like this.

```
v1 :: String
v1 = bindY
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y1 (WrapY 10 :: WrapY s Int)

v2 :: Int
v2 = bindY
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y2 (WrapY 10 :: WrapY s Int) (WrapY 20 :: WrapY s Int)
```

`v1` becomes `"110"`, and `v2` becomes `200`.

There is a problem though. Imagine you implemented an instance for a specific type manually.

```
type data BadKind = Bad

instance BindY Bad Int where
  get _ =
    DictY
      { _y1 = \n -> show $ n + 1000,
        _y2 = (+)
      }
```

Which implementation will it use when you get a value of `WrapY Bad Int` from `bindY` and apply `y1` to it outside `bindY`?

```
bad :: WrapY Bad Int
bad = bindY
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> WrapY 10 :: WrapY s Int

v5 :: String
v5 = y1 bad
```

`v5` becomes `"1010"` instead of `"110"`. It means that `y1 (WrapY 10 :: WrapY Bad Int)` returns a different result based on whether it's in a function passed to `bindY` or outside it. This breaks the referential transparency.

Fortunately, you can avoid this by existentially quantify `s` instead of universally quantify it.

```
bindY' :: forall a r. DictY a -> (forall k (s :: k). (BindY s a) => Proxy s -> r) -> r
bindY' dictY f =
  withDict
    @(BindY (Any @Any) a)
    @(Proxy (Any @Any) -> DictY a)
    (const dictY)
    (f @Any @Any)
    (Proxy :: Proxy s)
```

This makes it impossible to get a value of `WrapY s Int` from `bindY'` because `s` is now only available in a function passed to `bindY'`. You cannot call `y1` on `Wrap s Int` outside the function passed to `bindY'`.

Note that `@(BindY (Any @Any) a)` corresponds to `forall k (s :: k) (BindY s a) =>`, and `@(Proxy (Any @Any) -> DictY a)` corresponds to `forall k (s :: k). Proxy s -> DictY a`. Since `s` is existential, we use `Any` for it. Also we pass `@Any` to `Any` since `s` is polly-kinded. The `@Any` means any kind. In addition to that, we need to make `f` `f @Any @Any`. The first `@Any` corresponds to `k`, and the second `@Any` corresponds to `s`.

```
bad' :: WrapY Bad Int
bad' = bindY'
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> WrapY 10 :: WrapY Bad Int

v6 :: String
v6 = y1 bad'
```

Even though you can get `WrapY Bad Int` from `bindY'`, it's different from `WrapY s Int` we'd use in the function. We cannot use `s` used in the function to use our custom dictionary outside it. For example, this won't compile.

```
bad'' :: WrapY s Int
bad'' = bindY'
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> WrapY 10 :: WrapY s Int
```
