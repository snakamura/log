# Type of optics, part 3

So now, we have `Getter` and `Setter`. Let's try putting them together.

In [the part 1 of this series](https://snakamura.github.io/log/2023/9/optics1.html), we defined `Getter` in terms of `Profunctor`, but let's use the version not being generalized to `Profunctor`.

```
type Getter s a = forall f. (Functor f, Contravariant f) => (a -> f a) -> (s -> f s)
type Setter s t a b = forall f. Identical f => (a -> f b) -> (s -> f t)
```

When you extract a common factor from these two types, you'll get this.

```
type Lens1 s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
```

Next, we're going to put `to` and `setting` together.

```
to :: (s -> a) -> Getter s a
to get = \afb -> \s ->
  let a = get s
      fb = afb a
   in () <$ fb $< ()

setting :: ((a -> b) -> (s -> t)) -> Setter s t a b
setting map = \afb -> \s ->
  let ab a = extract (afb a)
      st = map ab
      t = st s
   in pure t
```


The function we're going to define takes `s -> a` and `(a -> b) -> (s -> t)` and returns `Lens1 s t a b`.

```
lens1 :: (s -> a) -> ((a -> b) -> (s -> t)) -> Lens1 s t a b
lens1 get map = \afb -> \s ->
  let a = get s
      fb = afb a
      ft = fmap (\b -> let ab _ = b
                           st = map ab
                           t = st s
                        in t) fb
   in ft
```

As you can see, we ignored the parameter when we converted `a` to `b`. So `(a -> b) -> (s -> t)` can be `b -> s -> t`.

```
lens1' :: (s -> a) -> (b -> s -> t) -> Lens1 s t a b
lens1' get set = \afb -> \s ->
  let a = get s
      fb = afb a
      ft = fmap (\b -> let st = set b
                           t = st s
                        in t) fb
   in ft
```

What do we do here? We get a value `a` from `s`, and pass it to `afb`. So if we use `Const a b` as `afb`, we can get this value `a` out of `fb`. This happens when we use this lens as a getter.

One the other hand, when we use `Identity b`, we can get `s -> t` from `b` in `fb` to get `ft`, from which we can extract `t`.

We'll get an ordinal `lens` function by swapping `b` and `s` in `b -> s -> t`.

```
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set afb s = fmap (set s) (afb (get s))
```

Since `Lens` is more generic than `Getter` and `Setter`, you can pass it to any actions taking `Getting` or `Setting` such as `view` and `over`.

Then, what happens when we put `Fold` and `Setter` together?

```
type Fold s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)
type Setter s t a b = forall f. Identical f => (a -> f b) -> (s -> f t)
```

Again, let's extract a common factor from these two types. You'll get `Traversal`.

```
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)
```

Then, let's try putting these functions together.

```
folding :: Foldable g => (s -> g a) -> Fold s a
folding fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
   in fb $< ()

setting :: ((a -> b) -> (s -> t)) -> Setter s t a b
setting map = \afb -> \s ->
  let ab a = extract (afb a)
      st = map ab
      t = st s
   in pure t
```

`folding` takes `s -> g a` to convert `s` to `Foldable g => g a`. Then instead of passing this function, let's assume that `s` itself is `Foldable g => g a`. Then, you'll get this `folding'`.

```
folding' :: Foldable g => Fold (g a) a
folding' = \afb -> \ga ->
  let fb = traverse_ afb ga
   in fb $< ()
```

Unfortunately, we cannot unify it with `setting` with `Foldable`, and we need `Traversable`. By making it use `Traversable` instead of `Foldable`, you'll get `traversing`. As you can see `traversing` is identical to `traverse` itself.

```
traversing :: Traversable g => Traversal (g a) (g b) a b
traversing = \afb -> \ga ->
  let fgb = traverse afb ga
   in fgb

traversing' :: Traversable g => Traversal (g a) (g b) a b
traversing' = traverse
```

You can do the same thing to `setting`. When you replace `s` with `g a` and `t` with `g b` where `g` is `Traversable`, you'll get `Traversable g => ((a -> b) -> (g a -> g b)) -> Setter (g a) (g b) a b`. When you look at this type, you'll find that `(a -> b) -> (g a -> g b)` is a type of `fmap` for `g`, and `g` is actually an instance of `Functor` (as it's an instance of `Traversable`).

When you remove this `map` parameter from `setting`, you'll get this `settings'`.

```
setting' :: Traversable g => Traversal (g a) (g b) a b
setting' = \afb -> \ga ->
  let fgb = traverse afb ga
   in fgb
```

Again, this is identical to `traverse`.

To put them together, we can say that any functor that is an instance of `Traversable` can be used as `Fold` and `Setter`, and you can pass it to actions taking `Getting` or `Setting` such as `view`, `views` and `over`.

Also, this gives you an intuition that 1) when you traverse a container with `Const`, you'll gather all its items by concatenating them as `Monoid`, and 2) when you traverse a container with `Identity`, you can apply a function `a -> b` to all its items without modifying the structure itself.
