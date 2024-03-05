# Playing with Sigma, part 1

As I wrote in [the previous post](https://snak.tumblr.com/post/632410914757492736/function-returning-some-types), you can think [Sigma](http://hackage.haskell.org/package/singletons-2.7/docs/Data-Singletons-Sigma.html#t:Sigma) is a generalized existential type. Then, how can I build it?

First, let's start from `SomeX` that contains type `X` indexed by kind `S`.

```
{-# LANGUAGE DataKinds,
             GADTs,
             PolyKinds,
             TemplateHaskell,
             TypeFamilies,
             TypeOperators
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind (Type)
import Data.Singletons.TH

singletons [d|
    data S = S1 | S2 | S3 | S4
  |]

data X (s :: S) = X

data SomeX where
    SomeX :: Sing s -> X s -> SomeX
```

Next, generialize it so that it can have any type indexed by `S`.

```
data Some1 (t :: S -> Type) where
    Some1 :: Sing s -> t s -> Some1 t
```

Okay, the indexing type isn't necessarily `S`. Let's allow any kind `k`.

```
data Some2 (t :: k -> Type) where
    Some2 :: Sing s -> t s -> Some2 t
```

Now `k` is inferred by the compiler, but let's specify it explicitly. Just like a data constructor can take a type now (as a singleton), a type constructor can now take a kind.

```
data Some3 k (t :: k -> Type) where
    Some3 :: Sing s -> t s -> Some3 k t
```

What kind will it be when you specify a kind explicitly? GHCi tells it's `forall k -> (k -> *) -> *`.

```
> :k Some3
Some3 :: forall k -> (k -> *) -> *
```

This `forall` means it's [a visible kind](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell). You cannot write this kind signature in the code in GHC 8.8, but you can write it using `StandaloneKindSignatures` extention in GHC 8.10.

Okay, next, make it use defunctionalized symbols so that you can pass a partially-applied type function to it.

```
data Some4 k (t :: k ~> Type) where
    Some4 :: Sing s -> t @@ s -> Some4 k t
```

Now, you'll find this `Some4` is equivalent to `Sigma`.
