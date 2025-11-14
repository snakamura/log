# Horizontal and vertical composition of natural transformations

Natural transformations are morphisms in a functor category where objects are functors. In Haskell, natural transformations between functors from `Hask` to `Hask` are expressed as polymorphic functions and often written using `~>`.

```
type f ~> g = forall x. f x -> g x
```

For example, a natural transformation from `Maybe` to `List` is written as `Maybe ~> List`.

There are two ways of composing natural transformations. The first one is called vertical composition and the second one is called horizontal composition. Let's see how they're different.

Imagine we have three functors; `(,) Bool`, `Maybe` and `List`, and two natural transformations; one from `(,) Bool` to `Maybe` and the other from `Maybe` to `List`.

```
pairToMaybe :: (,) Bool ~> Maybe
pairToMaybe (True, x) = Just x
pairToMaybe (False, _) = Nothing

maybeToList :: Maybe ~> List
maybeToList (Just x) = [x]
maybeToList Nothing = []
```

Vertical composition composes them naturally. It's just a composition of morphisms in the functor category. You can compose `(,) Bool ~> Maybe` and `Maybe ~> List` to get `(,) Bool ~> List`.

```
┌────┐                 ┌────┐
│    │ - [(,) Bool] -> │    │
│    │        ║        │    │
│    │  <pairToMaybe>  │    │
│    │        ⇩        │    │
│Hask│ --- [Maybe] --> │Hask│
│    │        ║        │    │
│    │  <maybeToList>  │    │
│    │        ⇩        │    │
│    │ --- [List] ---> │    │
└────┘                 └────┘
```

You can compose natural transformations vertically using the normal function composition (`(.)`).

```
(.|) ::
  (Functor f, Functor g, Functor h) =>
  (g ~> h) -> (f ~> g) -> (f ~> h)
gh .| fg = gh . fg

pairToList :: (,) Bool ~> List
pairToList = maybeToList .| pairToMaybe
```

Horizontal composition composes natural transformations by composing functors first.

```
┌────┐                                      ┌────┐
│    │ ---- [Compose Maybe ((,) Bool)] ---> │    │
│    │                 ┌────┐               │    │
│    │ - [(,) Bool] -> │    │ -- [Maybe]--> │    │
│    │        ║        │    │       ║       │    │
│Hask│  <pairToMaybe>  │Hask│ <maybeToList> │Hask│
│    │        ⇩        │    │       ⇩       │    │
│    │ --- [Maybe] --> │    │ --- [List] -> │    │
│    │                 └────┘               │    │
│    │ ------ [Compose List Maybe] -------> │    │
└────┘                                      └────┘
```

When you compose `(,) Bool` and `Maybe`, you'll get `Compose Maybe ((,) Bool)`. Also you'll get `Compose List Maybe` by composing `Maybe` and `List`. Now you can compose `pairToMaybe` and `maybeToList` somehow to get a natural transformation from `Compose Maybe ((,) Bool)` to `Compose List Maybe`.

```
(.-) ::
  (Functor f, Functor g, Functor j, Functor k) =>
  (j ~> k) -> (f ~> g) -> (Compose j f ~> Compose k g)
jk .- fg = \(Compose jfx) -> Compose (jk (fmap fg jfx))

type PairMaybe = Compose Maybe ((,) Bool)
type MaybeList = Compose List Maybe

pairMaybeToMaybeList :: PairMaybe ~> MaybeList
pairMaybeToMaybeList = maybeToList .- pairToMaybe
```

As you can see, we lift the first natural transformation (`pairToMaybe`) using `fmap` to apply it to `Compose Maybe ((,) Bool)`, then apply the second natural transformation (`maybeToList`) to get a composed natural transformation.

Note that you can compose natural transformations horizontally even when a target of the first natural transformation is different from a source of the second natural transformation. I mean the type of `(.-)` isn't `(g ~> h) -> (f ~> g) -> (Compose g f ~> Compose h g)`, but `(j ~> k) -> (f ~> g) -> (Compose j f ~> Compose k g)`.
