# Hom functor

When you have objects `P` and `Q` in a category `C`, there are zero or more morphisms from `P` to `Q`. A set of these morphisms are called a hom-set and written as `C(P, Q)`. (This is true only when `C` is locally small, but let's forget about it now).

Since a hom-set is a set, it consists a category. Each object in this category is a hom-set such as `C(P, Q)`. Let's call this category `D`.

If there are `R` and `S` in `C` in addition to `P` and `Q`, we have also `C(P, R)`, `C(P, S)`, `C(Q, R)` and so on in `D`, but let's focus on hom-sets whose origin is `P` such as `C(P, P)`, `C(P, Q)`, `C(P, R)`, `C(P, S)` now.

Since `C` and `D` are categories, there can be functors from `C` to `D`. One of these functors maps `Q` in `C` to `C(P, Q)` in `D`, and maps `R` in `C` to `C(P, R)` in `D`, and so on. It also maps morphisms from `Q` to `R` in `C` to morphisms from `C(P, Q)` to `C(P, R)` in `D`. This functor is called a hom-functor, and denoted by `C(P, -)`.

If `C` is a cartesian closed category like `Hask`, there are exponential objects such as `[P, Q]` and `[P, R]` that are isomorphic to `C(P, Q)` and `C(P, R)`. In this case, a hom-functor maps `Q` to `[P, Q]`, so is an endo functor. We call this kind of hom-functor an internal hom-functor.

In Haskell, a set of morphisms from `P` to `Q` is expressed as an exponential object (or hom-object) `P -> Q`, and `(->) P` is an internal hom-functor. `(->) P` maps `Q` to `P -> Q`, and maps morphisms `Q -> R` to `(P -> Q) -> (P -> R)` by function composition.

We can have another set of hom-functors by fixing a destination instead of an origin. This functor maps `Q` in `C` to `C(Q, P)`, and `R` to `C(R, P)`, for example, and denoted by `C(-, P)`. As you might've expected, this functor is a contravariant functor as oppose to a covariant functor `C(P, -)`.

In Haskell, it's expressed as [`Data.Functor.Contravariant.Op`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Functor-Contravariant.html#t:Op) and instance of [`Data.Functor.Contravariant.Contravariant`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Functor-Contravariant.html#t:Contravariant).
