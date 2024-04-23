# Implementing `Maybe` using dependent types in Idris

In [Implementing Maybe using dependent types](../../2020/11/dependent_maybe.html), we implemented Maybe using dependent types in Haskell. Now, let's try it in Idris.

Since Idris supports dependent types, it should be much easier.

The data type declarations are pretty much similar, but we don't need to promote types to kinds nor generate singleton types. We can just use types.

```
import Data.String

data O = S | N

data Optional : O -> Type -> Type where
    Some : a -> Optional S a
    None : Optional N a
```

The implementation of `textToInt` is very similar, too. But its type declaration is much simpler because we don't need defunctionalized symbols.

Also, we don't need to specify the first component of `DPair` as the complier can infer it.

```
textToInt : String -> (o ** Optional o Int)
textToInt s = case parseInteger {a=Int} s of
                Just n => (_ ** Some n)
                Nothing => (_ ** None)
```

The implementation of `addText` is straightforward.

```
addText : Int -> String -> (o ** Optional o Int)
addText n s = case textToInt s of
                (_ ** Some m) => (_ ** Some (n + m))
                (_ ** None) => (_ ** None)
```

In the Haskell version, we used `mapSigma` to map functions over a dependent pair (`Sigma`). Let's write `mapSigma` in Idris, then. Its implementation and type are almost identical to `mapSigma`.

```
mapDPair : (f : a -> b) -> ({x : a} -> p x -> q (f x)) -> DPair a p -> DPair b q
mapDPair f g (o ** x) = (f o ** g x)
```

Now, armed with `mapDPair`, you can write `addText'` like this.

```
addText' : Int -> String -> (o ** Optional o Int)
addText' n s = mapDPair id f $ textToInt s
  where
    f : Optional o Int -> Optional o Int
    f (Some m) = Some $ n + m
    f None = None
```

Of course, by making `Optional` an instance of `Functor`, you can use it as well.

```
implementation Functor (Optional o) where
    map f (Some n) = Some (f n)
    map _ None = None

addText'' : Int -> String -> (o ** Optional o Int)
addText'' n s = mapDPair id (map (+ n)) $ textToInt s
```

It's very nice that we don't need to add lots of type annotations to them unlike Haskell and singletons.
