# Limits to ends

Imagine that there is a discrete category $J$ with two objects and there are two functors from $J$ to $C$; constant functor $Delta(V)$ which maps both objects to $V$ in $C$, and functor $D$ that maps objects in $J$ to $P$ and $Q$ in $C$ respectively. [The limit of $D$ is $P \otimes Q$](./limits_adjunctions.html), which is `(P, Q)` if $C$ is $Hask$.

When you add more and more objects to $J$ so that it has all objects in $C$, the limit of $D$ will be a product of all objects in $C$. It's `forall a. a` if $C$ is $Hask$. Indeed we have projections from `forall a. a` to any type.

```
p :: (forall a. a) -> b
p a = a
```

We can apply this to functors from $Hask$ to $Hask$. For example, the limit of `Identity` functor is `forall a. a`, and the limit of `Maybe` functor is `forall a. Maybe a`, and so on.

But we cannot apply this to types that are not functors such as `a -> String` or `a -> a`.

When you think that `a -> String` is a functor from $Hask^{op}$ to $Hask$, you can think about its limit, which is a colimit of a contravariant functor from $Hask$ to $Hask$. Since colimit is a coproduct of the diagram, the colimit of `a -> String` is `exists a. a -> String`, which is written as `data Exists = forall a. Exists (a -> String)` in Haskell.

This represents the adjunction we saw in [A parameter type is existential, a return type is universal, part 1](../../2025/6/existential_universal1.html). You'll see the adjunction triple we saw in [A parameter type is existential, a return type is universal, part 4](../2025/7/existential_universal4.html) when we place a limit on the right and a colimit on the left.

But what can we do with `a -> a`? It's neither a covariant functor nor a contravariant functor. It's when [a profunctor comes into play](./profunctors_ends.html). As we saw in the post, the end of profunctor `a -> a` is `forall a. a -> a`.

When you recall that any functors can be profunctors, you can think that a limit of a functor is an end of that profunctor. For example, you can make any instance of `Functor` an instance of `Profunctor` by ignoring its contravariant part `s -> a`.

```
newtype WrappedFunctor f a b = WrappedFunctor (f b)

instance (Functor f) => Profunctor (WrappedFunctor f) where
  dimap :: (s -> a) -> (b -> t) -> (WrappedFunctor f a b -> WrappedFunctor f s t)
  dimap _ b2t (WrappedFunctor fb) = WrappedFunctor $ fmap b2t fb
```

An end of `WrappedFunctor f a a` is a limit of `f a`.

$$
\int_a WrappedFunctor f \, a \, a \cong \int_a f \, a \cong \lim_a f \, a
$$

The same goes for contravariant functors. You can make any instance of `Contravariant` an instance of `Profunctor` by ignoring its covariant part `b -> t`.

```
newtype WrappedContravariant f a b = WrappedContravariant (f a)

instance (Contravariant f) => Profunctor (WrappedContravariant f) where
  dimap :: (s -> a) -> (b -> t) -> (WrappedContravariant f a b -> WrappedContravariant f s t)
  dimap s2a _ (WrappedContravariant fa) = WrappedContravariant (contramap s2a fa)
```

The end of `WrappedContravariant f a a` is a colimit of `f a`.

$$
\int_a WrappedContravariant f \, a \, a \cong \int_a f \, a \cong \lim_{a \in Hask^{op}} f \, a \cong \operatorname*{colim}_a f \, a
$$

This is why they say ends are generalized limits.

It's also an idea to think about it by using a constant functor as `f` or `g` in a hom-set from `f a` to `g b`. This hom-set is an instance of `Profunctor`.

```
type Hom :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
newtype Hom f g a b = Hom (f a -> g b)

instance (Functor f, Functor g) => Profunctor (Hom f g) where
  dimap :: (s -> a) -> (b -> t) -> (Hom f g a b -> Hom f g s t)
  dimap s2a b2t (Hom h) = Hom (fmap b2t . h . fmap s2a)
```

When you use `Const c` as `f`, `Hom (Const c) g a` will be an instance of `Functor`.

```
instance (Functor g) => Functor (Hom (Const c) g a) where
  fmap :: (b -> t) -> (Hom (Const c) g a b -> Hom (Const c) g a t)
  fmap = rmap
```

Also, using `Const c` as `g` and swapping `a` and `b` makes it an instance of `Contravariant`.

```
newtype HomOp f g a b = HomOp (Hom f g b a)

instance (Functor f) => Contravariant (HomOp f (Const c) b) where
  contramap :: (s -> a) -> (HomOp f (Const c) b a -> HomOp f (Const c) b s)
  contramap s2a (HomOp hom)= HomOp (lmap s2a hom)
```

So when you pick a type in $Hask$ and another type in $Hask^{op}$, and map them to $Hask$, you can map one of them to one type in the target category using a constant functor. Then, the hom-set will be a covariant or contravariant functor. The end of this profunctor is a limit of the covariant functor as well as a colimit of the contravariant functor.

While ends are generalization of limits, coends are generalization of colimits. A coend of `a -> a` is `exists a. a -> a`, and a coend of `WrappedFunctor f a a` is a colimit of `f`.

$$
\int^a WrappedFunctor f \, a \, a \cong \int^a f \, a \cong \operatorname*{colim}_a f \, a
$$

Also, a coend of `WrappedContravariant f a a` is a limit of `f`.

$$
\int^a WrappedContravariant f \, a \, a \cong \int^a f \, a \cong \operatorname*{colim}_{a \in Hask^{op}} f \, a \cong \lim_a f \, a
$$
