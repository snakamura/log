# Mapping a function over a tuple, part 2

In [the previous post](./tuple_map1.html), we saw how we can or cannot map a function over a tuple in TypeScript. Let's do the same in Haskell in this post.

The first thing we need to do is defining a literal type since there is no literal type in Haskell.

```
type Literal :: Symbol -> Type
data Literal s = Literal

getLiteral :: forall (s :: Symbol). (KnownSymbol s) => Literal s -> String
getLiteral Literal = symbolVal (Proxy @s)

pattern Lit :: forall (s :: Symbol). (KnownSymbol s) => String -> Literal s
pattern Lit s <- (getLiteral -> s)

{-# COMPLETE Lit #-}
```

This type doesn't have any value in it, but you can get a string value from it using `symbolValue`. For example, you can use it like this.

```
let a = Literal @"a"
case a of
  Lit s -> putStrLn s
```

It'd be nice if we could make `Literal` use a visible forall, but it seems that GHC doesn't support it yet. You need to make it a function if you'd like to use it.

```
mkLiteral :: forall (s :: Symbol) -> Literal s
mkLiteral s = Literal @s
```

Then, you can pass a type-level symbol `"a"` (not a term-level string value `"a"`) like a normal parameter.

```
let a = mkLiteral "a"
```

Let's define `Object` next.

```
type Object :: Type -> Type
newtype Object nameType = Object
  { name :: nameType
  }
```

This is almost equivalent to this type in TypeScript, but without `extends string`. In Haskell, we don't specify constraints on types but on functions.

```
type Object<TName extends string> = {
  readonly name: TName
}
```

Next, we need a heterogeneous list. We can use a normal tuple such as `(Object (Literal "a"), Object (Literal "b"), Object (Literal "c"))`, but it's hard to process this recursively. It gets easier to process it using a nested tuple like `(Object (Literal "a"), (Object (Literal "b"), Object (Literal "c")))`, and our heterogeneous list represents this structure.

```
type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr `HCons`
```

We can now define a list of objects using them.

```
exampleObjects ::
  HList
    [ Object (Literal "a"),
      Object (Literal "b"),
      Object (Literal "c")
    ]
exampleObjects =
  Object {name = Literal @"a"}
    `HCons` Object {name = Literal @"b"}
    `HCons` Object {name = Literal @"c"}
    `HCons` HNil
```

Now, what we want to have is something like this.

```
map ::
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList nameTypes
```

But it's not possible to have it as a normal function because it'll then need to work with any heterogeneous list. Instead, we'll make it a method of a typeclass.

```
type Map :: [Type] -> Constraint
class Map objectTypes where
  type ResultTypes objectTypes :: [Type]
  map ::
    (forall nameType. Object nameType -> nameType) ->
    HList objectTypes ->
    HList (ResultTypes objectTypes)
```

Then, we'll define two instances; one for an empty list and the other for non-empty lists.

```
instance Map '[] where
  type ResultTypes '[] = '[]
  map ::
    (forall nameType. Object nameType -> nameType) ->
    HList '[] ->
    HList '[]
  map _ HNil = HNil

instance
  (Map objectTypes) =>
  Map (Object nameType ': objectTypes)
  where
  type
    ResultTypes (Object nameType ': objectTypes) =
      nameType ': ResultTypes objectTypes
  map ::
    (forall nameType'. Object nameType' -> nameType') ->
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': ResultTypes objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)
```

Since this `map` takes a function using a rank-2 type, we can pass a polymorphic function. For example, we're going to pass `name` whose type is `forall nameType. Object nameType -> nameType`.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
```

A type of `mappedName` becomes `HList [Literal "a", Literal "b", Literal "c"]` as you expected.

But wait, since `map` uses `Object` directly, this `map` cannot be used with a list of objects of another type. How can we support that? We can make it a type parameter of `Map` as well.

```
type Map :: (Type -> Type) -> [Type] -> Constraint
class Map objectTypeCon objectTypes where
  type ResultType objectTypes :: [Type]
  map ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    HList objectTypes ->
    HList (ResultType objectTypes)
```

Now, we'll pass `Object` type constructor to `Map` as well when we use `map` over a list of `Object`s.

```
instance Map objectTypeCon '[] where
  type ResultType '[] = '[]
  map ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    HList '[] ->
    HList '[]
  map _ HNil = HNil

instance
  (Map Object objectTypes) =>
  Map Object (Object nameType ': objectTypes)
  where
  type
    ResultType (Object nameType ': objectTypes) =
      nameType ': ResultType objectTypes
  map ::
    (forall nameType'. Object nameType' -> nameType') ->
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': ResultType objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)
```

You can use this new version in the same way with a list of `Object`s.

```
mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
```

Let's define another object type and see how we can use `map` over a list of objects of that type.

```
type Object' :: Type -> Type -> Type
data Object' ageType titleType = Object'
  { age :: ageType,
    title :: titleType
  }

exampleObjects' ::
  HList
    [ Object' Int (Literal "x"),
      Object' Int (Literal "y"),
      Object' Int (Literal "z")
    ]
exampleObjects' =
  Object' {age = 20, title = Literal @"x"}
    `HCons` Object' {age = 30, title = Literal @"y"}
    `HCons` Object' {age = 40, title = Literal @"z"}
    `HCons` HNil
```

When you define an instance for `Object'`, you can apply `map` over `exampleObjects'`.

```
instance
  (Map (Object' ageType) objectTypes) =>
  Map (Object' ageType) (Object' ageType titleType ': objectTypes)
  where
  type ResultType (Object' ageType titleType ': objectTypes) =
    titleType ': ResultType objectTypes
  map ::
    (forall titleType'. Object' ageType titleType' -> titleType') ->
    HList (Object' ageType titleType ': objectTypes) ->
    HList (titleType ': ResultType objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)
```

```
mappedTitles :: HList [Literal "x", Literal "y", Literal "z"]
mappedTitles = map (title @Int) exampleObjects'
```

Note that you need to fix `ageType` when you pass `title` to `map`. The type of `title` is `forall ageType titleType. Object' ageType titleType -> titleType`, and it's too polymorphic to be passed to `map`.

But don't you think it's a bit annoying that you need to define a whole `map` for each type? We'll see what we can do with it in the next post.
