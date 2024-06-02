# Category of monoids and monoid homomorphisms

What are objects and morphisms in the category of monoids in Haskell?

The objects are types that implement `Monoid` type class. The morphisms are functions that satisfy some conditions.

Imagine that you have types `M` and `N` that are instances of `Monoid` type class. First, a morphism has to map `mempty` of `M` to `mempty` of `N` (`f (mempty @M) = mempty @N`). Second, it has to map values `x` and `y` of `M` to `f x` and `f y` of `N` so that `f (x <> y) = f x <> f y`.

Let's take an example. We'll pick a list of `Char` (`[Char]`) and `Sum Int`. Then `length` is a morphism from `[Char]` to `Sum Int`.

You can see both `length (mempty @[Char])` and `getSum (mempty @(Sum Int))` is `0`. Also you can see both `length (['A', 'B'] <> ['C', 'D', 'E'])` and `getSum (Sum (length ['A', 'B']) <> Sum (length ['C', 'D', 'E']))` is `5`.

These functions are said to preserve monoidal structure and called monoid homomorphisms.
