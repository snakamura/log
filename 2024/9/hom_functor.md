# Hom functor

When you have objects `P` and `Q` in a category `C`, there are zero or more morphisms from `P` to `Q`. A set of these morphisms are called a hom-set and written as `C(P, Q)`. (This is true only when `C` is locally small, but let's forget about it now).

Since a hom-set is a set, it can be regarded as an object in the category Set. Each object in this category is a hom-set such as `C(P, Q)`. We'll refer to this category as `D` though it's just Set.

If there are `R` and `S` in `C` in addition to `P` and `Q`, we have also `C(P, R)`, `C(P, S)`, `C(Q, R)` and so on in `D`, but let's focus on hom-sets whose origin is `P` such as `C(P, P)`, `C(P, Q)`, `C(P, R)`, `C(P, S)` now.

Since `C` and `D` are categories, we can define a functor from `C` to `D`. It maps `Q` in `C` to `C(P, Q)` in `D`, and maps `R` in `C` to `C(P, R)` in `D`, and so on. It also maps morphisms from `Q` to `R` in `C` to morphisms from `C(P, Q)` to `C(P, R)` in `D`. This functor is called a hom-functor, and denoted by `C(P, -)`.

If `C` is a cartesian closed category like `Hask`, there are exponential objects such as `[P, Q]` and `[P, R]` that are isomorphic to `C(P, Q)` and `C(P, R)`. In this case, a hom-functor maps `Q` in `Hask` to `[P, Q]` in `Hask`, so we can say it's an endo functor. We call this kind of hom-functor an internal hom-functor.

In Haskell, the morphisms from `P` to `Q` is expressed as an exponential object (or hom-object) `P -> Q`. The type constructor `(->) P` defines an internal hom-functor. `(->) P` maps `Q` to `P -> Q`, and maps functions `Q -> R` to `(P -> Q) -> (P -> R)` by composing functions `f :: Q -> R` after `g :: P -> Q` as `f . g`.

We can have another set of hom-functors by fixing a destination instead of an origin. This functor maps `Q` in `C` to `C(Q, P)`, and `R` to `C(R, P)`, for example, and denoted by `C(-, P)`. As you might've expected, this functor is a contravariant functor as oppose to a covariant functor `C(P, -)`.

In Haskell, it's expressed as [`Data.Functor.Contravariant.Op`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Functor-Contravariant.html#t:Op) and instance of [`Data.Functor.Contravariant.Contravariant`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Functor-Contravariant.html#t:Contravariant).
