# Type of optics, part 1

When you use one of the optic libraries such as [lens](https://hackage.haskell.org/package/lens), or [lens-family](https://hackage.haskell.org/package/lens-family), you should've known that they used lots of types in the form of `(a -> f b) -> (s -> f t)` or even more general `p a (f b) -> p s (f t)`. Optics use these types so that they compose well, but it's hard to find where they came from at first. In this series, I'm going to learn where these types came from.

First of all, it'd help us to think that there are two kinds of types. The first kind is generic types. For example, in `lens`, you'll see types like this.

```
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
```

The second kind is concrete types. For example, there is `Getting` (also called `AGetter`) like this.

```
type Getting r s a = (a -> Const r a) -> s -> Const r s
```

As you can see, `Getter` uses generic `f` while `Getting` uses concrete `Const` as a functor.

Let's see how we use optics to understand this. We use them like these.

```
view _1 (1, 2) -- 1
set _1 'x' (1, 2) -- ('x', 2)
```

We call `_1` an optic, `view` and `set` actions, `(1, 2)` a structure, and `1` a focus. So an optic puts a focus on something in the structure, and an action does something on it. For example, `_1` puts a focus on the first value in a pair `(1, 2)`, and `view` gets the value itself. `_1` puts a focus on the first value in a pair `(1, 2)`, and `set` sets another value there, and so on.

We usually use `s` and `t` for types of structures, and `a` and `b` for types of focuses.

The basic principle here is that we use a generic type for an optic, but an action applies a concrete type to it. When you see a generic type such as `Getter`, you can think its a type of an optic itself. When you see a concrete type such as `Getting`, you can think it appears in parameters of an action.

You can see this by checking these types.

```
_1 :: Getter (a, b) a
view :: Getting a s a -> s -> a
```

Okay, now, let's start with the simplest one, a getter.

A getter is an optic to get a value of a focus from a structure. You can build this type of an optic from a function from `s` to `a`. For example, you can build a getter `_1` that focuses the first element of a pair from `fst`. Let's think about what type it'll be.

First, we'll convert a function `s -> a` to a different form. If you can get `a` from `s`, you can say that you can get a function `s -> r` for some `r` by passing a function `a -> r`. So the first version will be this one
```
to0 :: (s -> a) -> ((a -> r) -> (s -> r))
to0 get = \ar -> \s ->
  let a = get s
      r = ar a
   in r
```

Now, as `Const r a` is isomorphic to `r` (because its value `Const r` only carries a value of `r`), you can expand the type above to `(s -> a) -> ((a -> Const r b) -> (s -> Const r t))`. As you've already noticed, the right hand side is now in the form of `(a -> f b) -> (s -> f t)`.

The implementation will be very similar to `to0`, but it extracts a value from `Const r b` and packs it again in `Const r t`.

```
type Getter1 r s t a b = (a -> Const r b) -> (s -> Const r t)

to1 :: (s -> a) -> Getter1 r s t a b
to1 get = \afb -> \s ->
  let a = get s
      Const r = afb a
   in Const r
```

With `Getter1`, we can write the first version of `view` which extracts the original `get` from this getter.

```
view1 :: Getter1 a s t a b -> (s -> a)
view1 getter = \s ->
  let afb a = Const a
      sft = getter afb
      Const a = sft s
   in a
```

From here, let's make the type of `to` more generic. The first thing we'll do is using constraints `(Functor f, Contravariant f)` instead of using `Const`. The constraints `(Functor f, Contravariant f)` means it's a both covariant and contravariant functor. This can be possible only when `fmap` and `contramap` does nothing, so it means `f` is isomorphic to `Const r`.

```
instance Functor (Const r) where
  fmap _ (Const r) = Const r

instance Contravariant (Const r) where
  contramap _ (Const r) = Const r
```

As you can see, calling `fmap` or `contramap` on `Const r` doesn't change its value, but only changes its type. Let's see how the implementation will be.

```
type Getter2 r s t a b = forall f. (Functor f, Contravariant f) => (a -> f b) -> (s -> f t)

to2 :: (s -> a) -> Getter2 r s t a b
to2 get = \afb -> \s ->
  let a = get s
      fb = afb a
   in () <$ fb $< ()
```

What's this `() <$ fb $< ()`? You'll get `(contramap (const ()) . fmap (const ())) fb` when you expand this. The type of `fb` is `f b` for arbitrary `b`, and we want to convert it to `f t` for arbitrary `t`.

The type of `fmap (const ())` is `Functor f => f a -> f ()`, so by applying `fmap (const ())` to `fb`, you'll get a value of type `f ()`.

Next, the type of `contramap (const ())` is `Contravariant f => f () -> f a`. By applying `contramap (const ())` to it, you'll get a value of type `f t` for arbitrary `t`.

As we saw above, applying `fmap` and `contramap` to a functor that is isomorphic to `Const r` doesn't change its value. So this `() <$ fb $< ()` is a fancy way of converting `Const r b` to `Const r t` without changing its value.

Now, we can simplify this a bit by removing unnecessary type variables `r`, `b` and `t`. This is because you'll use a getter only to get a value of `a` from `s`.

```
type Getter3 s a = forall f. (Functor f, Contravariant f) => (a -> f a) -> (s -> f s)

to3 :: (s -> a) -> Getter3 s a
to3 get = \afb -> \s ->
  let a = get s
      fb = afb a
   in () <$ fb $< ()
```

When you think this from a bit different perspective, you can see that we are now converting `a -> f a` (`afa`) to `s -> f s`, and we have `s -> a` (`get`) in our hand. When we compose `get` and `afa`, you'll get `afa . get :: s -> f a`. Also, you'll find that the type of `contramap get` is `f a -> f s`. So you can compose it as well to get `contramap get . afa . get :: s -> f s`.

```
to3' :: (s -> a) -> Getter3 s a
to3' get = \afa -> contramap get . afa . get
```

This can be further generalized to `Profunctor`. You can think a `Profunctor a b` a generalized function type `a -> b`. In `to3'`, we apply `get` to an input of the function `afa`, and apply `contramap get` to its output. We can express this using `dimap`.

```
type Getter4 s a = forall p f. (Profunctor p, Functor f, Contravariant f) => p a (f a) -> p s (f s)

to4 :: (s -> a) -> Getter4 s a
to4 get = \pafa -> dimap get (contramap get) pafa
```

This is one of the most generalized forms of a getter and `to` function that builds a getter.

Let's get back to `view` action now. We use `Getter1` when we defined `view1`, but we can no longer use `Getter4` because it was generalized. We need to define `Getting` separately.

```
type Getting2 s a = (a -> Const a a) -> (s -> Const a s)

view2:: Getting2 s a -> (s -> a)
view2 getter = getConst . getter Const
```

Now that we've defined `Getter`, `Getting` and `to` which creates a getter from a function `s -> a`, let's see what we can do with a function `s -> [a]` next.

We have a function `getList :: s -> [a]`, and try to create a getter. We need to get `a` from `[a]` somehow. The simplest way is to use `mconcat` assuming `a` is an instance of `Monoid`.

```
list1 :: Monoid a => (s -> [a]) -> Getter1 a s t a b
list1 getList = \afb -> \s ->
  let la = getList s
      fb = afb (mconcat la)
      Const a = fb
   in Const a
```

We'll use a bit of trick here. We'll take advantage of the fact that `Const` accumulates values when you traverse a list with it. For example, `traverse_ Const ["a", "b", "c]` will be `Const "abc"`.

By applying this trick to `list1`, we'll get this `list1'`.

```
list1' :: forall s t a (b :: Type). Monoid a => (s -> [a]) -> Getter1 a s t a b
list1' getList = \afb -> \s ->
  let la = getList s
      fb = traverse_ afb la
      Const a = fb
   in Const a
```

Note that because `Const` is poly-kinded, we need `forall s t a (b :: Type)` to make it clear that `b` is a type, but you can ignore its detail for now.

Okay, let's try making this more generic.

First, let's generalize `list1'` and use `Foldable` instead of a list. In `list1'`, we use a function `s -> [a]`, but we should be able to generalize it to `s -> g a` for any `Foldable g` now that we use `traverse_` to traverse a container.

```
type Fold1 s t a b = (a -> Const a b) -> (s -> Const a t)

folding1 :: forall g s t a (b :: Type). (Foldable g, Monoid a) => (s -> g a) -> Fold1 s t a b
folding1 fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
      Const a = fb
   in Const a
```

As we did with `to`, we'll generalize this from `Const` to a generic functor with some constraints. The first step is removing unused type variables.

```
type Fold2 s a = (a -> Const a a) -> (s -> Const a s)

folding2 :: forall g s a. (Foldable g, Monoid a) => (s -> g a) -> Fold2 s a
folding2 fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
      Const a = fb
   in Const a
```

Then, stop depending directly on `Const`.

```
type Fold3 s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

folding3 :: Foldable g => (s -> g a) -> Fold3 s a
folding3 fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
   in fb $< ()
```

We need `Applicative f` in addition to `Functor f` and `Contravariant f` because we pass `afb` to `traverse_`. Also, since `fb` is already `f ()`, we can apply `$<` (`contramap (const ())`) to convert it to `f s` for arbitrary `s`.

Now that we have `folding`, let's see the type of `views`. `views` is very similar to `view`, but it takes a function to convert each focused value to another value. Since `views` is an action, it'll take an optic with a concrete type.

```
type Folding2 r s a = (a -> Const r a) -> (s -> Const r s)

views2 :: Folding2 r s a -> (a -> r) -> s -> r
views2 fold f s =
  let afa a = Const (f a)
      sfs = fold afa s
      Const r = sfs
   in r
```

You can shorten it by composing these functions.

```
views2' :: Folding2 r s a -> (a -> r) -> s -> r
views2' fold f = getConst . fold (Const . f)
```

You'll find they're very similar when you compare `Folding2` with `Getting2`. `Getting2 s a` is identical to `Folding2 a s a`. So let's put them together to get `Getting3`, and define `view3` and `views3` in terms of `Getting3`.

```
type Getting3 r s a = (a -> Const r a) -> (s -> Const r s)

view3 :: Getting3 a s a -> s -> a
view3 getter = getConst . getter Const

views3 :: Getting3 r s a -> (a -> r) -> s -> r
views3 fold f = getConst . fold (Const . f)
```

As you can see, `view3` and `views3` are very similar. The only difference is that the latter takes a function `a -> r` to concatenate values in another form of `Monoid`.

For example, you'll gather all the focused values as a list.

```
toListOf3 :: Getting3 [a] s a -> s -> [a]
toListOf3 fold = views3 fold pure
```

This works well, but you can make it more efficient by using `Data.Monoid.Endo` as I wrote in [When do you use Data.Monoid.Endo?](https://snak.tumblr.com/post/679221762414837760/when-do-you-use-datamonoidendo).

```
toListOf3' :: Getting3 (Endo [a]) s a -> s -> [a]
toListOf3' fold s = appEndo (views3 fold (Endo . (:)) s) []
```

Also, you can implement `preview` to get the first item if available with `Data.Monoid.First`.

```
preview3 :: Getting3 (First a) s a -> s -> Maybe a
preview3 fold s = getFirst (views3 fold (First . pure) s)
```

By putting them together, we'll have these types and functions.

```
type Getter s a = forall p f. (Profunctor p, Functor f, Contravariant f) => p a (f a) -> p s (f s)

type Fold s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

type Getting r s a = (a -> Const r a) -> (s -> Const r s)

to :: (s -> a) -> Getter s a
to get = dimap get (contramap get)

folding :: Foldable g => (s -> g a) -> Fold s a
folding fold afb s = traverse_ afb (fold s) $< ()

view :: Getting a s a -> s -> a
view getter = getConst . getter Const

views :: Getting r s a -> (a -> r) -> s -> r
views fold f = getConst . fold (Const . f)

preview :: Getting3 (First a) s a -> s -> Maybe a
preview fold s = getFirst (views fold (First . pure) s)

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf fold s = appEndo (views fold (Endo . (:)) s) []
```

`Getter` and `Fold` are types of optics themselves, and `Getting` is a type for actions that take an optic of type `Getter` and `Fold` (there are actually more types though).
