# Monoidal functor and `Applicative`

In the category of Haskell types `Hask`, objects are types and morphisms are functions. For instance `Int` and `Bool` are objects and functions from `Int` to `Bool` are all morphisms. When you look into the inside of `Int`, you can say that `Int` is a set whose elements are values `1`, `2`, `3` and so on.

There are lots of morphisms from `Int` to `Bool`, and when you think them as elements of a set, you now get a type `Int -> Bool` whose elements are these morphisms. This kind of types are called an exponential object. You can say that function types are exponential objects in `Hask`.

Unfortunately, this definition cannot be applied directly to other categories because it uses elements of a set, which we cannot look into in more general categories.

Instead, we'll define an exponential object in a bit different way in any monoidal category. In a monoidal category `(C, ⊗, I)`, `[A, B]` is an exponential object if there exists a natural isomorphism `C(X ⊗ A, B) ≅ C(X, [A, B])`. But, what does this mean?

Imagine that you have an object `A` and `B` in a monoidal category. You'll pick any object `X` and create a tensor product `X ⊗ A`. Then, think about a set of morphisms from `X ⊗ A` to `B`. It's `C(X ⊗ A, B)`. Next, imagine you have an object `[A, B]`. You'll pick any object `X` and think about a set of morphisms from `X` to `[A, B]`. It's `C(X, [A, B])`. You can say that `[A, B]` is an exponential object if these two morphisms are isomorphic.

As a side note, a functor that maps `B` to `[A, B]` is called an internal hom-functor an denoted as `[A, -]`.

When you apply this to the monoidal category `(Hask, (,), ())`, `X ⊗ A` is `(X, A)`, and `[A, B]` is `A -> B`. So `A -> B` is an exponential object if we can say that functions from `(X, A)` to `B` are isomorphic to functions from `X` to `A -> B`. Indeed, we can say they're isomorphic by `curry` and `uncurry`. Also, a functor `(->) A` is an internal hom-functor.

A category with this structure is called a monoidal closed category. It's called a cartesian closed category if its tensor product is a cartesian product and its unit is a terminal object. Since `(,)` is a cartesian product and `()` is a terminal object in `Hask`, we can say that the monoidal category `(Hask, (,), ())` is a cartesian closed category.

In [the previous post](../6/monoidal_functor.html), we saw that a monoidal functor preserving a monoidal structure of `(Hask, (,), ())` is equivalent to `Applicative`. This means that this monoidal functor preserves exponential objects.

What does it mean when you say a functor preserves exponential objects? Imagine you have objects `A`, `B` and their exponential object `A -> B`. When you apply a functor `f` to `A -> B`, you'll get `f (A -> B)`. On the other hand, when you first apply `f` to `A` and `B` you'll get `f A` and `f B`, then get their exponential object `f A -> f B`. If these two are isomorphic, this functor is said to preserve exponential objects.

As we saw in the previous post, a monoidal functor preserves a monoidal structure, which means you have `(f a, f b) -> f (a, b)`.

Imagine that we have `A`, `B` and `C` in the source category. We know that `(A, B) -> C` is isomorphic to `A -> (B -> C)` because the source category is a closed monoidal category.

Since the functor preserves the hom-set isomorphism between `(A, B) -> C` and `A -> (B -> C)`, we can say that `f (A, B) -> f C` is isomorphic to `f A -> f (B -> C)`. Also this functor is a monoidal functor, we can say `(f A, f B) -> f C` is isomorphic to `f A -> f (B -> C)` by applying `(f a, f b) -> f (a, b)`.

In the target category, which is the same category as the source category, we know that `(f A, f B) -> f C` is isomorphic to `f A -> (f B -> f C)` because the target category is also a closed monoidal category.

By putting them together, we can say that `f A -> f (B -> C)` is isomorphic to `f A -> (f B -> f C)`, which means that `f (B -> C)` is isomorphic to `f B -> f C`. Since the functor is actually a lax monoidal functor which only have `(f a, f b) -> f (a, b)` not `f (a, b) -> (f a, f b)`, we say we have `f (B -> C) -> (f B -> f C)`.

As you might've already noticed, this type is the type of `(<*>)` in `Applicative`, and `Applicative` is a functor preserving exponential objects.

This derivation looks a bit confusing in Haskell syntax because we use `->` for both morphisms and exponential objects. It might be easier to understand it by writing like this.

1. **C**(A ⊗ B, C) ≅ **C**(A, [B, C])
    - Hom-set from `A ⊗ B` to `C` in **C** is isomorphic to hom-set from `A` to `[B, C]` in **C** because **C** is a monoidal closed category
    - **C** is a source category
    - `⊗` is a tensor product in **C**
    - `[B, C]` is an exponential object
2. **D**(F(A ⊗ B), F(C)) ≅ **D**(F(A), F([B, C]))
    - A functor preserves hom-set isomorphism
    - **D** is a target category
    - `F` is a functor
3. **D**(F(A) ⊗' F(B), F(C)) ≅ **D**(F(A), F([B, C]))
    - A functor preserves monoidal products
    - `⊗'` is a tensor product in **D**
4. **D**(F(A) ⊗' F(B), F(C)) ≅ **D**(F(A), [F(B), F(C)])
    - Hom-set from `F(A) ⊗' F(B)` to `F(C)` in **D** is isomorphic to hom-set from `F(A)` to `[F(B), F(C)]` in **D** because **D** is a monoidal closed category
5. **D**(F(A), F([B, C])) ≅ **D**(F(A), [F(B), F(C)])
    - From 3 and 4
6. F([B, C]) ≅ [F(B), F(C)]
    - From 5
