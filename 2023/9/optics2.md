# Type of optics, part 2

In [the part 1 of this series](https://snakamura.github.io/log/2023/9/optics1.html), we gave a look at `Getter`, `Fold` and `Getting`. In this part, we'll look at `Setter` and `Setting` (also called `ASetter`).

When we built a getter, we built it from a function `s -> a`. What's the fundamental operation of a setter then? It's a function `(a -> b) -> (s -> t)`. For example, when we have a function increment an integer `(+ 1) :: Int -> Int`, that function will be a function that applies it to the first element of a pair and returns `(Int, a) -> (Int, a)` when `s` and `t` are a pair.

Then, how can we build a function in a form `(a -> f b) -> (s -> f t)` from `(a -> b) -> (s -> t)`? It's simple! `Identity a` is isomorphic to `a`, so we can just use `Identity` as `f`. Now, we get `(a -> Identity b) -> (s -> Identity t)`. Let's call it `Setter1`.

The function to convert `(a -> b) -> (s -> t)` to `(a -> Identity b) -> (s -> Identity t)` just wraps a value in `Identity` and unwraps a value from `Identity`.

```
type Setter1 s t a b = (a -> Identity b) -> (s -> Identity t)

setting1 :: ((a -> b) -> (s -> t)) -> Setter1 s t a b
setting1 map = \afb -> \s ->
  let ab a = runIdentity (afb a)
      st = map ab
      t = st s
   in Identity t
```

You can extract `(a -> b) -> (s -> t)` from `Setter1 s t a b` like this. It's the well-known `over` action.

```
over1 :: Setter1 s t a b -> (a -> b) -> (s -> t)
over1 setter = \ab -> \s ->
  let afb a = Identity (ab a)
      sft = setter afb
      Identity t = sft s
   in t
```

Let's make `Setter1` more generic like we did with `Getter`. When you see `setting1`, you'll find that we need a function `a -> f a` an `f a -> a`. We can use `Applicative` for the former, but there seems no good candidate for the latter. So let's define our own.

```
type Identical2 :: (Type -> Type) -> Constraint
class Identical2 f where
  extract2 :: f a -> a

instance Identical2 Identity where
  extract2 = runIdentity
```

With `Identical2`, we can make `Setter1` more generic.

```
type Setter2 s t a b = forall f. (Functor f, Applicative f, Identical2 f) => (a -> f b) -> (s -> f t)

setting2 :: ((a -> b) -> (s -> t)) -> Setter2 s t a b
setting2 map = \afb -> \s ->
  let ab a = extract2 (afb a)
      st = map ab
      t = st s
   in pure t
```

`over` action has to use `Identity` directly, so we'll give it another type.

```
type Setting2 s t a b = (a -> Identity b) -> (s -> Identity t)

over2 :: Setter1 s t a b -> (a -> b) -> (s -> t)
over2 setter = \ab -> \s ->
  let afb a = Identity (ab a)
      sft = setter afb
      Identity t = sft s
   in t
```

The actual `Identical` class in `lens-family` has one more constraint `Traversable`. This is because you can freely define `traverse` in terms of `Identical2` and `Applicative`.

```
identicalTraverse :: (Identical2 f, Applicative f, Functor g) => (a -> g b) -> f a -> g (f b)
identicalTraverse agb fa =
  let a = extract2 fa
      gb = agb a
      gfb = fmap pure gb
   in gfb
```

Even though adding `Traversable` constraint adds nothing in practice, it'll make it even clearer that a functor implementing this class is isomorphic to `Identity`.

You can read a bit more about isomorphism to `Identity` in [Traversable, Lone and Distributive](https://snak.tumblr.com/post/723546375430733824/traversable-lone-and-distributive).

By applying this constraint, we'll get `Identical3`.

```
type Identical3 :: (Type -> Type) -> Constraint
class (Applicative f, Traversable f) => Identical3 f where
  extract3 :: f a -> a

instance Identical3 Identity where
  extract3 = runIdentity

type Setter3 s t a b = forall f. Identical3 f => (a -> f b) -> (s -> f t)

setting3 :: ((a -> b) -> (s -> t)) -> Setter3 s t a b
setting3 map = \afb -> \s ->
  let ab a = extract3 (afb a)
      st = map ab
      t = st s
   in pure t
```

To put them together, we'll get these types and functions.

```
type Identical :: (Type -> Type) -> Constraint
class (Applicative f, Traversable f) => Identical f where
  extract :: f a -> a

instance Identical Identity where
  extract = runIdentity

type Setter s t a b = forall f. Identical f => (a -> f b) -> (s -> f t)

type Setting s t a b = (a -> Identity b) -> (s -> Identity t)

setting :: ((a -> b) -> (s -> t)) -> Setter s t a b
setting map afb = pure . map (extract . afb)

over :: Setting s t a b -> (a -> b) -> s -> t
over setter ab = runIdentity . setter (Identity . ab)
```

You might wonder it was worth generalizing `Setter`, but it allows us to define an optic that works both as `Getter` and `Setter`, and more.
