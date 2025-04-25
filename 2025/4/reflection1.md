# Implementing an instance of a type class dynamically, part 1

We have `elem` function in `base` whose type is `Eq a => a -> [a] -> Bool` if you specialized it to a list (The actual type is `(Foldable t, Eq a) => a -> t a -> Bool`). Now, is it possible to write a `elemBy` function whose type is `(a -> a -> Bool) -> a -> [a] -> Bool` using `elem`?

`elem` calls `(==) :: a -> a -> Bool` in `Eq` to check if an element is equal to a specified value. `elemBy` will use the first parameter of type `a -> a -> Bool` instead of `(==)`. Yes, we want to implement a temporary instance of `Eq` and let `elem` use it.

A straightforward way is to declare a type that is a pair of a value and a function, and implement `Eq` for it.

```
data WrapEq a = WrapEq a (a -> a -> Bool)

instance Eq (WrapEq a) where
  (WrapEq lhs eq) == (WrapEq rhs _) = eq lhs rhs

elemBy' :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy' eq e l = elem (WrapEq e eq) $ map (`WrapEq` eq) l
```

This works, but there is a problem even if we put performance issues aside. As you can see, the implementation of `(==)` uses a function on the left-hand side, and ignores a function on the right-hand side assuming we've put the same function to all the elements. But what happens if we put different functions to each element?

I'm going to look into what we can do about it.

To make it simpler, I'll define one simple type class named `X`, and try to implement this type class dynamically instead of `Eq`.

```
class X a where
  x :: a -> String

instance X String where
  x = id

instance X Int where
  x = show
```

Of course, we can invoke `x` like this.

```
invoke :: (X a) => a -> String
invoke a = x a

v1 :: String
v1 = invoke (100 :: Int) <> "!!!"
```

Let's make it CPS-style.

```
invokeCPS :: (X a) => a -> (a -> r) -> r
invokeCPS a f = f a

v2 :: String
v2 = invokeCPS (100 :: Int) $ \n -> x n <> "!!!"
```

Let's give `\n -> x n <> "!!!"` a look. A type of this function is `(X a) => a -> String`. Internally, a type class constraint `(X a) =>` will be converted to an additional parameter of a dictionary of methods in GHC. So its type will be `Dict X a -> a -> String` for some `Dict`. In addition to that, `Dict X a` is a function itself if the type class only has one method. To put them together, the function type becomes identical to `(a -> String) -> a -> String`.

This means that you can call this function with any function of `a -> String` if you can cast `(X a) => a -> String` to `(a -> String) -> a -> String`. We can do that by wrapping it in a `newtype` and `unsafeCoerce` it.

```
newtype Magic a r = Magic ((X a) => a -> r)

unsafeInvoke :: a -> ((X a) => a -> r) -> r
unsafeInvoke a f = unsafeCoerce (Magic f) (\n -> show (n + 1 :: Int)) a

v3 :: String
v3 = unsafeInvoke (100 :: Int) $ \n -> x n <> "!!!"
```

`v3` will be `"101!!!"` instead of `"100!!!"`.

You can of course make `unsafeInvoke` take a function of type `a -> String` and use it.

```
unsafeInvoke2 :: (a -> String) -> a -> ((X a) => a -> r) -> r
unsafeInvoke2 x' a f = unsafeCoerce (Magic f) x' a

v4 :: String
v4 = unsafeInvoke2 (\n -> show (n + 2)) (100 :: Int) $ \n -> x n <> "!!!"
```

Now, once you've passed a function `\n -> show (n + 2)` to `unsafeInvoke2`, it'll use this function as an implementation of `x` of `X`. `v4` will be `"102!!!"` as you expected.

Another option instead of using unsafe `unsafeCoerce` is using `GHC.Exts.withDict`. It does what `unsafeCoerce` does in a safe manner.

```
safeInvoke :: forall a r. (a -> String) -> a -> ((X a) => a -> r) -> r
safeInvoke x' a f = withDict @(X a) x' (f a)

v5 :: String
v5 = safeInvoke (\n -> show (n + 3)) (100 :: Int) $ \n -> x n <> "!!!"
```

They work pretty well, but they only work with a type class with one method. What can we do with a type class with more than one method? We'll see it in the following posts.
