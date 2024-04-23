# Type-safe list with `GHC.TypeLits`

In [the previous post](http://snak.tumblr.com/post/109752112942/type-safe-list-datakinds-constraintkinds-and), I've defined a type-safe list using [Peano numbers](http://wiki.haskell.org/Peano_numbers), but with [`GHC.TypeLits`](http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html#t:Nat), you don't need to define your own type level numbers.

In short, when you import `GHC.TypeLits`, it defines `Nat` kind and types like `0`, `1`, `2`, `3`, and so on. It also defines operators such as `+`, `-`, etc that work on the types. Note that these are types and type operators, not values and operators.

Now, we can write a type-safe list like this. To use these features, you need to enable `DataKinds` and `TypeOperators` extensions.

    {-# LANGUAGE DataKinds,
                 GADTs,
                 KindSignatures,
                 NoImplicitPrelude,
                 StandaloneDeriving,
                 TypeOperators #-}

    import GHC.TypeLits (Nat, type(+), type(-), type( List l a -> List (l + 1) a

    deriving instance Show a => Show (List len a)

    head :: (1  List len a -> a
    head (Cons a _) = a

But when you define `tail`, you'll find that it doesn't type-check with an error.

    tail :: (1  List len a -> List (len - 1) a
    tail (Cons _ r) = r

    l5.hs:22:19:
        Could not deduce (l ~ (len - 1))
        from the context (1  List len a -> List (len - 1) a
          at l5.hs:21:9-52
        or from (len ~ (l + 1))
          bound by a pattern with constructor
                     Cons :: forall a (l :: Nat). a -> List l a -> List (l + 1) a,
                   in an equation for ‘tail’
          at l5.hs:22:7-14
          ‘l’ is a rigid type variable bound by
              a pattern with constructor
                Cons :: forall a (l :: Nat). a -> List l a -> List (l + 1) a,
              in an equation for ‘tail’
              at l5.hs:22:7
        Expected type: List (len - 1) a
          Actual type: List l a
        Relevant bindings include
          r :: List l a (bound at l5.hs:22:14)
          tail :: List len a -> List (len - 1) a (bound at l5.hs:22:1)
        In the expression: r
        In an equation for ‘tail’: tail (Cons _ r) = r

This is because GHC 7.8 is not smart enough to understand that `(len + 1) - 1 == len`. Good news is that GHC 7.10 will support this.

Meanwhile, we have to use `unsafeCoerce` to have GHC know that these types are the same.

    tail :: (1  List len a -> List (len - 1) a
    tail (Cons _ r) = unsafeCoerce r
