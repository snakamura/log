# Type of optics, part 4

We've seen `Getter`, `Fold`, `Setter` and `Traversal`. Let's see a prism at the end. A prism is an optic that allows you to get zero or one value, but always allows you to set a value. In this sense, you can think it as one of `Traversal`s.

Let's start with a prism as `Traversal`. The fundamental operations is to get `a` from `s` which can fail, and build `t` from `b`.

```
type Prism1 s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

prism1 :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1 sa bt = \afb -> \s ->
  case sa s of
    Left t -> pure t
    Right a ->
      let fb = afb a
       in fmap bt fb
```

This is straightforward. Let's transform it a bit.

```
prism1' sa bt = \afb -> \s -> either pure (fmap bt . afb) (sa s)
prism1'' sa bt = \afb -> either pure (fmap bt . afb) . sa
prism1''' sa bt = \afb -> (either pure (fmap bt) . fmap afb) . sa
prism1'''' sa bt = \afb -> either pure (fmap bt) . fmap afb . sa
prism1''''' sa bt = \afb -> dimap sa (either pure (fmap bt)) (fmap afb)
prism1'''''' sa bt = dimap sa (either pure (fmap bt)) . fmap
```

We transformed an explicit `case` to `either`, then transformed function compositions to `dimap` of `Profunctor`.

Even though a prism created by this function works as `Traversal`, we cannot `review` it. We need to get `b -> t` out of a prism to do that.

Let's make the type signature a bit more generic. By replacing the last `fmap` with `right'`, we can make its type more generic with `Profunctor` and `Choice`. Note that in the `Choice` instance for `(->)`, `right'` is identical to `fmap`.

```
type Prism2 s t a b = forall p f. (Profunctor p, Choice p, Applicative f) => p a (f b) -> p s (f t)

prism2 :: (s -> Either t a) -> (b -> t) -> Prism2 s t a b
prism2 sa bt = dimap sa (either pure (fmap bt)) . right'
```

To get `b -> t` from `Prism2`, we'll instantiate a prism with `Tagged` and `Identity`.

```
type APrism2 s t a b = Tagged a (Identity b) -> Tagged s (Identity t)

review2 :: APrism2 s t a b -> b -> t
review2 prism = \b ->
  let pafb = Tagged $ Identity b
      psft = prism pafb
      t = runIdentity $ unTagged psft
   in t
```

In `Tagged a b`, `a` is a phantom type, and its `Profunctor` instance ignores the first component in `dimap`. It's like `fmap` for `Const a` ignoring its component.

```
instance Profunctor Tagged where
  dimap _ f (Tagged s) = Tagged (f s)

instance Functor (Const a) where
  fmap _ (Const a) = Const a
```

This makes it possible for `review2` to ignore `sa` part in `prism2` and extract `bt` from it.

To put them together, we'll get `prism` to create a prism and `review` action.

```
type Prism s t a b = forall p f. (Profunctor p, Choice p, Applicative f) => p a (f b) -> p s (f t)

type APrism s t a b = Tagged a (Identity b) -> Tagged s (Identity t)

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism sa bt = dimap sa (either pure (fmap bt)) . right'

review :: APrism s t a b -> b -> t
review prism = runIdentity . unTagged . prism . Tagged . Identity
```
