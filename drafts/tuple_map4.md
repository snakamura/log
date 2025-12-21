# Mapping a function over a tuple, part 4

In this series of posts, we wanted to define this function, and used typeclasses.

```
map ::
  (forall elementType. objectConType elementType -> elementType) ->
  HList objectTypes ->
  HList nameTypes
```

In this post, we'll use type witnesses to define this function.

First, we need witness types.

```
type IsObject :: Type -> Type
data IsObject objectType where
  IsObject :: IsObject (Object nameType)

type AreObjects :: [Type] -> Type
data AreObjects objectTypes where
  AreObjectsNil :: AreObjects '[]
  AreObjectsCons ::
    IsObject objectType ->
    AreObjects objectTypes ->
    AreObjects (objectType ': objectTypes)
```

`IsObject` guarantees that `objectType` is `Object nameType` for some `nameType`, and `AreObjects` guarantees that `objectTypes` are a list of `objectType`s where `IsObject objectType` exists.

We need a small utility type family `ResultTypes` to get a result type of `map`.

```
type ResultTypes :: [Type] -> [Type]
type family ResultTypes objectTypes where
  ResultTypes '[] = '[]
  ResultTypes (Object nameType ': objectTypes) =
    nameType ': ResultTypes objectTypes
```

With the help of `ResultTypes` utility, you can write `map` like this.

```
map ::
  AreObjects objectTypes ->
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (ResultTypes objectTypes)
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons IsObject areObjects) f (HCons object objects) =
  HCons
    (f object)
    (map areObjects f objects)
```

This means that `map` can map a function over `objectTypes` as long as there is a value of `AreObjects objectTypes`. Let's call it with `exampleObjects`.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames =
  map
    ( AreObjectsCons
        IsObject
        ( AreObjectsCons
            IsObject
            (AreObjectsCons IsObject AreObjectsNil)
        )
    )
    name
    exampleObjects
```

The type of this `AreObjects` is `AreObjects '[Object nameType1, Object nameType2, Object nameType3]`. When you pass it to `map`, `objectTypes` in `map` will be `'[Object nameType1, Object nameType2, Object nameType3]`. This makes a type of an input list `HList '[Object nameType1, Object nameType2, Object nameType3]`. Now, it knows that all contents of this list are `Object`, it can call a function of type `(forall nameType. Object nameType -> nameType)` on them safely.

Of course, you can build this type witness from `sampleObjects` using typeclasses.

```
type IsObjectC :: Type -> Constraint
class IsObjectC objectType where
  isObject :: objectType -> IsObject objectType

instance IsObjectC (Object nameType) where
  isObject :: Object nameType -> IsObject (Object nameType)
  isObject _ = IsObject

type AreObjectsC :: [Type] -> Constraint
class AreObjectsC objectTypes where
  areObjects :: HList objectTypes -> AreObjects objectTypes

instance AreObjectsC '[] where
  areObjects :: HList '[] -> AreObjects '[]
  areObjects HNil = AreObjectsNil

instance
  (IsObjectC objectType, AreObjectsC objectTypes) =>
  AreObjectsC (objectType ': objectTypes)
  where
  areObjects ::
    HList (objectType ': objectTypes) ->
    AreObjects (objectType ': objectTypes)
  areObjects (HCons object objects) =
    AreObjectsCons (isObject object) (areObjects objects)
```

Once you've defined a function `mapC` that use these type classes,

```
mapC ::
  (AreObjectsC objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (ResultTypes objectTypes)
mapC f objects = map (areObjects objects) f objects
```

you can call it without passing a type witness explicitly.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapC name exampleObjects
```

There are two problems though. The first one is that `map` depends on `Object` directly. The second one is that `ResultTypes` isn't generic.

In [the previous post](./tuple_map3.html), we implemented generic `MapTypes` by defunctionalizing type families. In this post, we'll remove `ResultTypes` by making `IsObject` take a result type in addition to an object type.

```
type IsObject :: Type -> Type -> Type
data IsObject objectType nameType where
  IsObject :: IsObject (Object nameType) nameType

type AreObjects :: [Type] -> [Type] -> Type
data AreObjects objectTypes nameTypes where
  AreObjectsNil :: AreObjects '[] '[]
  AreObjectsCons ::
    IsObject objectType nameType ->
    AreObjects objectTypes nameTypes ->
    AreObjects (objectType ': objectTypes) (nameType ': nameTypes)
```

Regarding the first problem, you'll pass a polymorphic function taking `IsObject` to `map` to make `map` independent from `Object` directly.

```
map ::
  AreObjects objectTypes nameTypes ->
  ( forall objectType nameType.
    IsObject objectType nameType ->
    objectType ->
    nameType
  ) ->
  HList objectTypes ->
  HList nameTypes
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons isObject areObjects) f (HCons object objects) =
  HCons
    (f isObject object)
    (map areObjects f objects)
```

You now need to pass a polymorphic function to `map` that can handle any `IsObject`.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames =
  map
    ( AreObjectsCons
        IsObject
        ( AreObjectsCons
            IsObject
            (AreObjectsCons IsObject AreObjectsNil)
        )
    )
    (\IsObject object -> name object)
    exampleObjects
```

When you want to use this `map` with `Object'`, you need to add a constructor to `IsObject`.

```
type IsObject :: Type -> Type -> Type
data IsObject objectType elementType where
  IsObject :: IsObject (Object nameType) nameType
  IsObject' :: IsObject (Object' ageType titleType) titleType
```

Once you've done that, you need to pass a function that accepts any constructor of `IsObject` to `map`.

```
mappedTitles :: HList [Literal "x", Literal "y", Literal "z"]
mappedTitles =
  map
    ( AreObjectsCons
        IsObject'
        ( AreObjectsCons
            IsObject'
            (AreObjectsCons IsObject' AreObjectsNil)
        )
    )
    ( \cases
        IsObject object -> name object
        IsObject' object' -> title object'
    )
    exampleObjects'
```

But you also need to update `mappedNames` because now you need to pass a function that can handle `IsObject'` as well to `map`. This is because a list now can contain both `Object` and `Object'`, and the function has to handle both cases.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames =
  map
    ( AreObjectsCons
        IsObject
        ( AreObjectsCons
            IsObject
            (AreObjectsCons IsObject AreObjectsNil)
        )
    )
    ( \cases
        IsObject object -> name object
        IsObject' object' -> title object'
    )
    exampleObjects
```

This works, but it's not ideal because we need to pass a function that can handle both `Object` and `Object'` even when you know that a list only contains `Object` or `Object'`.

It's an idea to avoid this by passing a list of functions instead of passing a polymorphic function to `map` just like we did with TypeScript in [the first post of this series](./tuple_map1.html).

```
type Arrows :: [Type] -> [Type] -> [Type]
type family Arrows xs rs where
  Arrows '[] '[] = '[]
  Arrows (x ': xs) (r ': rs) = (x -> r) ': Arrows xs rs

map ::
  AreObjects objectTypes elementTypes ->
  HList (Arrows objectTypes elementTypes) ->
  HList objectTypes ->
  HList elementTypes
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons _ areObjects) (HCons f fs) (HCons object objects) =
  HCons
    (f object)
    (map areObjects fs objects)
```

Note that `Arrows` can be defined with [`ZipWith` in `singletons`](https://hackage.haskell.org/package/singletons-base-3.5/docs/Data-List-Singletons.html#t:ZipWith) as `type Arrows as rs = ZipWith (TyCon2 (->)) as rs`.

Once you've built a list of functions from a list of `Object`s and a polymorphic function using a typeclass,

```
class BuildObjectArrows objectTypes nameTypes where
  buildObjectArrows ::
    AreObjects objectTypes nameTypes ->
    (forall nameType. Object nameType -> nameType) ->
    HList (Arrows objectTypes nameTypes)

instance BuildObjectArrows '[] '[] where
  buildObjectArrows AreObjectsNil _ = HNil

instance
  (BuildObjectArrows objectTypes nameTypes) =>
  BuildObjectArrows (Object nameType ': objectTypes) (nameType ': nameTypes)
  where
  buildObjectArrows (AreObjectsCons IsObject areObjects) f =
    HCons f (buildObjectArrows areObjects f)
```

you can call `map` with it.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames =
  let areObjects =
        AreObjectsCons
          IsObject
          ( AreObjectsCons
              IsObject
              (AreObjectsCons IsObject AreObjectsNil)
          )
   in map
        areObjects
        (buildObjectArrows areObjects name)
        exampleObjects
```

You need to define another typeclass to build a list of functions for `Object'` though.

```
class BuildObject'Arrows objectTypes titleTypes where
  buildObject'Arrows ::
    AreObjects objectTypes titleTypes ->
    (forall ageType titleType. Object' ageType titleType -> titleType) ->
    HList (Arrows objectTypes titleTypes)

instance BuildObject'Arrows '[] '[] where
  buildObject'Arrows AreObjectsNil _ = HNil

instance
  (BuildObject'Arrows objectTypes titleTypes) =>
  BuildObject'Arrows (Object' ageType titleType ': objectTypes) (titleType ': titleTypes)
  where
  buildObject'Arrows (AreObjectsCons IsObject' areObjects) f =
    HCons f (buildObject'Arrows @objectTypes @titleTypes areObjects f)

mappedTitles :: HList [Literal "x", Literal "y", Literal "z"]
mappedTitles =
  let areObjects =
        AreObjectsCons
          IsObject'
          ( AreObjectsCons
              IsObject'
              (AreObjectsCons IsObject' AreObjectsNil)
          )
  in map
    areObjects
    (buildObject'Arrows areObjects title)
    exampleObjects'
```

You can of course build type witnesses using typeclasses here, too, but you still need to write a typeclass and its instances for each type you want to apply `map` to.

This is almost identical to what we did with TypeScript in [the first post](./tuple_map1.html). You need to write a lot more than in TypeScript because we casted to `any` at lot of places in TypeScript while this one is type-safe.
