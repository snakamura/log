# Limits and adjunctions

Imagine that you have a discrete category $J$, and category $C$. There are two objects `J1` and `J2` without morphisms except their identities in $J$, and you have a functor $F$ that maps `J1` and `J2` to `A` and `B` in $C$ respectively. The limit of $F$ is product $A \otimes B$.

Now we pick an object `V` in $C$ and have a functor $\Delta(V)$ from $J$ to $C$. $\Delta(V)$ maps both `J1` and `J2` to `V`. Since both $F$ and $\Delta(V)$ are functors from $J$ to $C$, they're objects in category $C^J$ of functors from $J$ to $C$. Morphisms in this category are natural transformations whose components are `V ~> A` and `V ~> B`, and you can write it $Hom_{C^J}(\Delta(V), F)$. This represents a cone from `V` to `A` and `B`.

Let's call a natural transformation from `V` to `A` `p'`, and `V` to `B` `q'`. When $F$ has a limit `Lim(F)`, there should be natural transformations from `Lim(F)` to `A` named `p` and `Lim(F)` to `B` named `q`. `p'` should be factorized to `p . h` for some `h`, and `q'` should be factorized to `q . h`.

This means that $Hom_{C^J}(\Delta(V), F)$ is isomorphic to `h` which is a hom-set from `V` to `Lim(F)` ($Hom_C(V, Lim(F))$). The intuition is that you can find a unique cone $Hom_{C^J}(\Delta(V), F)$ when you have `h` since `h` is a unique morphism from `V` to `Lim(F)`.

When you write this isomorphism this way,

$Hom_{C^J}(\Delta(V), F) \cong Hom_C(V, Lim(F))$

- $\Delta$ is a functor from $C$ to $C^J$
- $V$ is an object in $C$
- $F$ is a functor from $C^J$ to $C$
- $Lim$ is a functor from $C^J$ to $C$

and squint a bit, you'll find that it's in the form expressing adjunction.

$Hom_N(P X, Y) \cong Hom_M(X, Q Y)$

where

- $M$ is category $C$
- $N$ is category $C^J$ (category of functors from $J$ to $C$)
- $P$ is functor $\Delta$ from $C$ to $C^J$
- $Q$ is functor $Lim$ from $C^J$ to $C$
- $X$ is an object $V$ in $C$
- $Y$ is an object $F$ in $C^J$ (functor from $J$ to $C$)

You can say that the functor that maps $F$ to its limit is right adjoint to the functor that maps $V$ to the constant functor $\Delta(V)$.

Let's try writing it in Haskell, then. We'll use our new kind `J` representing the index category. `Delta` is a functor from $Hask$ to $Hask^J$ (`Type` to `J -> Type`), and `Lim` is a functor from $Hask^J$ to $Hask$ (`J -> Type` to `Type`).

```
type data J = J1 | J2

type Delta :: Type -> (J -> Type)
newtype Delta v a = Delta v

type Lim :: (J -> Type) -> Type
data Lim f = Lim (f J1) (f J2)
```

When you define `~>` to represent natural transformations (morphisms in functor category $Hask^J$ (`J -> Type`)), you can write a typeclass representing adjunction. `f` is a functor from $Hask$ to $Hask^J$ (`Type` to `J -> Type`), and `g` is a functor from $Hask^J$ to $Hask$ (`J -> Type` to `Type`).

```
type (~>) :: (J -> Type) -> (J -> Type) -> Type
type f ~> g = forall (j :: J). f j -> g j

type Adjunction :: (Type -> (J -> Type)) -> ((J -> Type) -> Type) -> Constraint
class Adjunction f g | f -> g, g -> f where
  leftAdjunct :: forall (v :: Type) (d :: J -> Type). (f v ~> d) -> (v -> g d)
  rightAdjunct :: forall (v :: Type) (d :: J -> Type). (v -> g d) -> (f v ~> d)
```

Since we only have two objects $J1$ and $J2$ in category $J$ (two types `J1` and `J2` of kind `J`), a polymorphic function `f v ~> d` (`forall (j :: J) :: f v j -> d j`) is equivalent to a pair of functions `(f v J1 -> d J1, f v J2 -> d J2)`. Expanding `j` this way makes it look like this.

```
type Adjunction' :: (Type -> (J -> Type)) -> ((J -> Type) -> Type) -> Constraint
class Adjunction' f g | f -> g, g -> f where
  leftAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (f v J1 -> d J1, f v J2 -> d J2) -> (v -> g d)
  rightAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (v -> g d) -> (f v J1 -> d J1, f v J2 -> d J2)
```

Now you can write an adjunction of `Delta` and `Lim`.

```
instance Adjunction' Delta Lim where
  leftAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (Delta v J1 -> d J1, Delta v J2 -> d J2) -> (v -> Lim d)
  leftAdjunct' (p, q) = \v -> Lim (p (Delta v)) (q (Delta v))

  rightAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (v -> Lim d) -> (Delta v J1 -> d J1, Delta v J2 -> d J2)
  rightAdjunct' f =
    ( \(Delta v) -> let Lim a _ = f v in a,
      \(Delta v) -> let Lim _ b = f v in b
    )
```

Let's take an example. We have a functor `D` that maps $J$ (`J`) to $Hask$ (`Type`) and an additional type `C`, and pick a triple of `D J1`, `D J2` and `C` (`V`) as `v` (an apex of the cone).

```
type D :: J -> Type
data family D j
data instance D J1
data instance D J2

data C

data V = V (D J1) (D J2) C
```

$Hom_{Hask^J}(\Delta(V), D)$ becomes `homDeltaVtoD`, and $Hom_{Hask}(V, Lim(D))$ becomes `homVtoLimD`.

```
homDeltaVtoD :: (Delta V J1 -> D J1, Delta V J2 -> D J2)
homDeltaVtoD = (\(Delta (V a _ _)) -> a, \(Delta (V _ b _)) -> b)

homVtoLimD :: V -> Lim D
homVtoLimD (V a b _) = Lim a b
```

You can apply `leftAdjunct'` to `homDeltaVtoD` to get `homVtoLimD`, and apply `rightAdjunct'` to `homVtoLimD` to get `homDeltaVtoD`.

```
homVtoLimD' :: V -> Lim D
homVtoLimD' = leftAdjunct' homDeltaVtoD

homDeltaVtoD' :: (Delta V J1 -> D J1, Delta V J2 -> D J2)
homDeltaVtoD' = rightAdjunct' homVtoLimD
```

This adjunction is the adjunction we saw in [A parameter type is existential, a return type is universal, part 2](../../2025/6/existential_universal2.html).
