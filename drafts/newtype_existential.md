# `newtype` with poly-kinded photom types

These days, `PolyKinds` and `RankNTypes` extensions are enabled by default with `GHC2024`. The former makes types poly-kinded as much as possible, which can cause some weird errors.

Imagine that you have a `newtype` `X`.

```
newtype X a = X a
```

You can declare another `newtype` `Y` to wrap it.

```
newtype Y a = Y (X a)
```

But when you add a phantom type `s` to `X`,

```
newtype X s a = X a
```

The compiler refuses `Y` which uses a rank-n type

```
newtype Y a = Y (forall s. X s a)
```

with this error.

```
<interactive>:1:15: error: [GHC-07525]
    • A newtype constructor must not have existential type variables
      Y :: forall a {k}. (forall (s :: k). X s a) -> Y a
    • In the definition of data constructor ‘Y’
      In the newtype declaration for ‘Y’
```

This looks weird because you didn't use any existential type variables.

This happens because `s` is poly-kinded. When you check `:t X` in GHCi, you'll find that `s` has an invisible kind `k` implicitly.

```
> :t X
X :: forall {k} (s :: k) a. a -> X s a
```

Since this kind isn't declared explicitly in `Y`, it makes GHC infer a type of `Y` as

```
newtype Y a = forall k. Y (forall (s :: k). X s a)
```

as the error message suggested, which makes `k` an existential type variable.

You can avoid this by declaring `k` explicitly along with `s` to make it not an existential variable.

```
newtype Y a = Y (forall k s. X (s :: k) a)
```
