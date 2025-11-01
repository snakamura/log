# Mapping a function over a tuple, part 3

We defined `map` in `Map` typeclass in [the previous post](./tuple_map2.html), and found that we needed to define entire `map` for each type. It'd be nice if we could reuse the implementation for different types. What can we do?

First, let's define a typeclass `MapItem` that maps each object. We'll implement an instance of this typeclass for each type instead of `Map`.

```
type MapItem :: Type -> Constraint
class MapItem objectType where
  type ResultType objectType :: Type
  mapItem ::
    (forall nameType. Object nameType -> nameType) ->
    objectType ->
    ResultType objectType
```

Then, we define its instance for `Object`.

```
instance MapItem (Object nameType) where
  type ResultType (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f
```

We now need two utilities to implement `map`. The first one is a type family mapping `objectTypes` to `nameTypes` using `ResultType`. The second one is mapping `objectTypes` to constraints `(MapItem objectType1, MapItem objectType2, ...)`.

```
type ResultTypes :: [Type] -> [Type]
type family ResultTypes ts where
  ResultTypes '[] = '[]
  ResultTypes (t ': ts) = ResultType t ': ResultTypes ts

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)
```

Note that you can use [`All` in generics-sop](https://hackage.haskell.org/package/generics-sop-0.5.1.4/docs/Generics-SOP.html#t:All) instead of this `All`.

Using `MapItem` and these utilities, you can now implement `map`.

```
map ::
  (All MapItem objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (ResultTypes objectTypes)
map _ HNil = HNil
map f (HCons object objects) = HCons (mapItem f object) (map f objects)
```

You can use it with `exampleObjects` like this.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
```

There are two problems though. The first problem is that `MapItem` uses `Object` directly, and the second problem is that `ResultTypes` uses `ResultType` directly. We can solve the first problem by making `MapItem` take a type constructor just like we did in the previous post, but how can we solve the second one?

I'd be great if we could define a type family like this.

```
type MapTypes :: (Type -> Type) -> [Type] -> [Type]
type family MapTypes f ts where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f t ': MapTypes f ts
```

And use `MapTypes ResultType objectTypes` instead of `ResultTypes objectTypes`. But unfortunately, type families don't support partial applications. So you cannot pass `ResultType` to `MapTypes`. We need to defunctionalize it.

The idea is that we define partially applied versions of a type family manually and have another type family that defines how to apply a type parameter to it.

```
type TyFun :: k -> l -> Type
data TyFun a b

type (~>) :: k -> l -> Type
type a ~> b = TyFun a b -> Type

type Apply :: (a ~> b) -> a -> b
type family Apply f x

type (@@) :: (a ~> b) -> a -> b
type f @@ x = Apply f x
infixr @@
```

For example, we can define `ResultTypeSym0` like this.

```
type ResultTypeSym0 :: Type ~> Type
data ResultTypeSym0 t

type instance Apply ResultTypeSym0 x = ResultType x
```

This means that you'll get `ResultType x` when you apply `x` to `ResultTypeSym0`.

`TyFun` is a promoted kind that marks a symbol we're going to use as a type function from `a` to `b`. Unfortunately, we cannot make `Apply` take `TyFun` directly because there is no type that belongs to `TyFun a b` kind.

When you define a type using `data`, its kind must be `... -> Type`. So we use a type whose kind is `TyFun a b -> Type` as a symbol for a type function. You can think `TyFun a b` is a kind of a phantom kind.

You can use [`singletons`](https://hackage.haskell.org/package/singletons) instead of defining them by yourself. It has `TyFun`, `(~>)`, `Apply` and `@@`, as well as a Template Haskell function to generate `ResultTypeSym0` from `ResultType`.

Now, let's define `MapTypes` using them.

```
type MapTypes :: (Type ~> Type) -> [Type] -> [Type]
type family MapTypes f ts where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f @@ t ': MapTypes f ts
```

This version takes `Type ~> Type` instead of `Type -> Type`, and uses `f @@ t` instead of `f t`. This change allows you to pass `ResultTypeSym0` to `MapTypes`.

By making `MapItem` take a type constructor, and making `map` use `MapTypes`, it'll be free from `Object`.

```
type MapTypes :: (Type ~> Type) -> [Type] -> [Type]
type family MapTypes f ts where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f @@ t ': MapTypes f ts

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

type MapItem :: (Type -> Type) -> Type -> Constraint
class MapItem objectTypeCon objectType where
  type ResultType objectType :: Type
  mapItem ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    objectType ->
    ResultType objectType

type ResultTypeSym0 :: Type ~> Type
data ResultTypeSym0 t

type instance Apply ResultTypeSym0 x = ResultType x

map ::
  All (MapItem objectType) objectTypes =>
  (forall nameType. objectType nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes ResultTypeSym0 objectTypes)
map _ HNil = HNil
map f (HCons x xs) = HCons (mapItem f x) (map f xs)
```

Once you've written an instance of `MapItem` for `Object`, you can use it to map `name` over a list of `Object`s.

```
instance MapItem Object (Object nameType) where
  type ResultType (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
```

To apply `map` to a list of `Object'`s, you just need to write an instance of `MapItem` for `Object'`.

```
instance MapItem (Object' ageType) (Object' ageType titleType) where
  type ResultType (Object' ageType titleType) = titleType
  mapItem ::
    (forall titleType'. Object' ageType titleType' -> titleType') ->
    Object' ageType titleType ->
    titleType
  mapItem f = f

mappedTitles :: HList [Literal "x", Literal "y", Literal "z"]
mappedTitles = map (title @Int) exampleObjects'
```

Now we only need to write an instance of `MapItem` for each type, which is more concise than defining entire `map` for each type.
