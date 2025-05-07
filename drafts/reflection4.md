# Implementing an instance of a type class dynamically, part 4

[We saw](./reflection3.html) how we can implement an instance of a type class dynamically. Now let's revisit it with a type class whose method returns an instance head, for example, `Monoid`.

```
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..), reify)

type Wrap :: k -> Type -> Type
newtype Wrap s a = Wrap a

type DictMonoid :: Type -> Type
data DictMonoid a = DictMonoid
  { _mempty :: a,
    _mappend :: a -> a -> a
  }

instance (Reifies s (DictMonoid a)) => Semigroup (Wrap s a) where
  (Wrap a1) <> (Wrap a2) = Wrap $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (Wrap s a) where
  mempty = Wrap $ _mempty (reflect (Proxy :: Proxy s))
```

You can use it like this.

```
v :: (Reifies s (DictMonoid Int)) => Wrap s Int
v = mempty <> Wrap 10 <> Wrap 20

v1 :: Int
v1 = reify (DictMonoid 0 (+)) $ \(_ :: Proxy s) -> let Wrap n = v @s in n

v2 :: Int
v2 = reify (DictMonoid 1 (*)) $ \(_ :: Proxy s) -> let Wrap n = v @s in n
```

As you can see, a function passed to `reify` cannot return `Wrap s a` because `s` is an existential type. It means that you always have to unwrap it. Let's define a new function that unwrap `Wrap s a` for convenience.

```
withDictMonoid :: forall a. DictMonoid a -> (forall k (s :: k). (Reifies s (DictMonoid a)) => Wrap s a) -> a
withDictMonoid dictY value = reify dictY $
  \(_ :: Proxy s) -> let Wrap v :: Wrap s a = value in v
```

`withDictMonoid` just unwraps a value returned from a function. Callers became a bit cleaner with it.

```
v3 :: Int
v3 = withDictMonoid (DictMonoid 0 (+)) v

v4 :: Int
v4 = withDictMonoid (DictMonoid 1 (*)) v
```

Can we generalize `withDictMonoid`? Yes, we can use a type family to associate a dictionary type and a constraint. Then, we can make `withDictMonoid` use a proper dictionary type.

```
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..), reify)

type ReifiableConstraint :: (Type -> Constraint) -> Constraint
class ReifiableConstraint c where
  type Dict c a :: Type

type Wrap :: k -> Type -> Type
newtype Wrap s a = Wrap {unwrap :: a}

withDict :: forall c a. Dict c a -> (forall k (s :: k). (Reifies s (Dict c a)) => Wrap s a) -> a
withDict dict value = reify dict $ \(_ :: Proxy s) -> unwrap (value :: Wrap s a)
```

This requires `AllowAmbiguousTypes` extension because `withDict` doesn't use `c` itself. It uses `c` only as `Dict c a` but the compiler cannot decide what `c` is from `Dict c a` because it's not injective. You need to specify a constraint when you call `withDict` like `withDict @Monoid`.

Then, let's define instances of `ReifiableConstriant` for `Semigroup` and `Monoid`.

```
type DictMonoid :: Type -> Type
data DictMonoid a = DictMonoid
  { _mempty :: a,
    _mappend :: a -> a -> a
  }

instance ReifiableConstraint Semigroup where
  type Dict Semigroup a = DictMonoid a

instance ReifiableConstraint Monoid where
  type Dict Monoid a = DictMonoid a

instance (Reifies s (DictMonoid a)) => Semigroup (Wrap s a) where
  (Wrap a1) <> (Wrap a2) = Wrap $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (Wrap s a) where
  mempty = Wrap $ _mempty (reflect (Proxy :: Proxy s))
```

You can now use `withDict` by specializing it to `Monoid` constraint.

```
v :: (Reifies s (DictMonoid Int)) => Wrap s Int
v = mempty <> Wrap 10 <> Wrap 20

v1 :: Int
v1 = withDict @Monoid (DictMonoid 0 (+)) v

v2 :: Int
v2 = withDict @Monoid (DictMonoid 1 (*)) v
```

We've evaluated `v` with different sets of methods of `Semigroup` and `Monoid`.

By the way, you can remove `AllowAmbiguousTypes` by making `Wrap` indexed by `c` as well if you'd like. Then, `c` will appear in the type signature of `withDict` as `Wrap c s a`, and it's no longer ambiguous.

```
type ReifiableConstraint :: (Type -> Constraint) -> Constraint
class ReifiableConstraint c where
  type Dict c a :: Type

type Wrap :: (Type -> Constraint) -> k -> Type -> Type
newtype Wrap c s a = Wrap {unwrap :: a}

withDict :: forall c a. Dict c a -> (forall k (s :: k). (Reifies s (Dict c a)) => Wrap c s a) -> a
withDict dict value = reify dict $ \(_ :: Proxy s) -> unwrap (value :: Wrap c s a)

type DictMonoid :: Type -> Type
data DictMonoid a = DictMonoid
  { _mempty :: a,
    _mappend :: a -> a -> a
  }

instance ReifiableConstraint Semigroup where
  type Dict Semigroup a = DictMonoid a

instance ReifiableConstraint Monoid where
  type Dict Monoid a = DictMonoid a

instance (Reifies s (DictMonoid a)) => Semigroup (Wrap Monoid s a) where
  (Wrap a1) <> (Wrap a2) = Wrap $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (Wrap Monoid s a) where
  mempty = Wrap $ _mempty (reflect (Proxy :: Proxy s))
```

You no longer need to pass `@Monoid` to `withDict` with this version because you've already specified it to `Wrap` as `Wrap Monoid s Int`.

```
v :: (Reifies s (DictMonoid Int)) => Wrap Monoid s Int
v = mempty <> Wrap 10 <> Wrap 20

v1 :: Int
v1 = withDict (DictMonoid 0 (+)) v

v2 :: Int
v2 = withDict (DictMonoid 1 (*)) v
```
