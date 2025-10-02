# Kleisli category and an adjunction

We saw an adjunction that gives rise to a monad `T` from a free T-algebra in [the previous post](../9/t_algebra_adjunction.html). Now let's take a look at another adjunction that gives rise to `T`.

We'll think about a monad `T` in `Hask`. There is a category called Kleisli category $Hask_T$ in which an object is the same object in `Hask` but a morphism $f :: A_{Hask_T} \to B_{Hask_T}$ is equivalent to `pure . f :: A -> T B` in `Hask` where `f :: A -> B` is a morphism in `Hask`.

When you have two morphisms $f_{Hask_T} :: A_{Hask_T} \to B_{Hask_T}$ and $g_{Hask_T} :: B_{Hask_T} \to C_{Hask_T}$ in $Hask_T$, their composition $g_{Hask_T} \circ f_{Hask_T} :: A_{Hask_T} \to C_{Hask_T}$ is equivalent to `join . fmap g . f :: A -> T C` in `Hask` where `f :: A -> T B` and `g :: B -> T C`. Note that `f` is equivalent to $f_{Hask_T}$, and `g` is equivalent to $g_{Hask_T}$.

There is a functor $F$ from `Hask` to $Hask_T$. This functor maps an object `A` in `Hask` to an object $A_{Hask_T}$ in $Hask_T$. It also maps a morphism `f :: A -> B` to $f :: A_{Hask_T} \to B_{Hask_T}$ which is equivalent to `pure . f :: A -> T B` as described above.

There is also a functor $U$ from $Hask_T$ to `Hask`. This functor maps an object $A_{Hask_T}$ to `T A`. Also, it maps a morphism $f :: A_{Hask_T} \to B_{Hask_T}$ (which is equivalent to `f :: A -> T B`) to `join . fmap f :: T A -> T B`.

There is an adjunction $F \dashv U$. $F$ is a left adjoint and $U$ is a right adjoint.

`unit` is a natural transformation $I \to U \circ F$. When you pick an object `A` in `Hask`, $F$ maps it to $A_{Hask_T}$ in $Hask_T$. Then, $U$ maps it to `T A` in `Hask`. `unit` will be a morphism `A -> T A` in `Hask` which is `pure` of `T`.

`counit` is a natural transformation $F \circ U \to I$. When you pick an object $A_{Hask_T}$ in $Hask_T$, $U$ maps it to `T A` in `Hask`. Then, $F$ maps it to $T A_{Hask_T}$ in $Hask_T$. `counit` will be $T A_{Hask_T} \to A_{Hask_T}$, which is equivalent to `T A -> T A` in `Hask` which is `id`.

$U \circ F$ produces the monad `T` itself.

When you think about a category of adjunctions that result in the monad `T` on `Hask`, this adjunction is the initial object in this category. Also, the free T-algebra adjunction we saw in the previous post is the terminal object.
