# Profunctors and ends

Imagine that you have functors `f` and `g`. When you map an object in their source category `a`, you'll get `f a` and `g a` in the target category, and there might be morphisms from `f a` to `g a`. Do we have a functor from the source category to the category of these morphisms? What kind of relationships does the functor represent?

If we have such functor, we should be able to write it like this in Haskell.

```
type Hom :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype Hom f g a = Hom (f a -> g a)

instance (Functor f, Functor g) => Functor (Hom f g) where
  fmap :: (a -> b) -> (Hom f g a -> Hom f g b)
  fmap = ???
```

This functor maps a type `a` to a hom-set `f a -> g a`. But you cannot write this instance. The best thing you could do is

```
fmap a2b (Hom fa2ga) =
  Hom $ \fb -> let fa = fb2fa? fb
                   ga = fa2ga fa
                   gb = fmap a2b ga
                in gb
```

but you cannot get this `fb2fa?` from `a2b`.

Even though there is no such functor, we can have a profunctor. Instead of taking a single type `a`, it takes two types `a` and `b` and maps them to `f a -> g b`.

```
type Hom :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
newtype Hom f g a b = Hom (f a -> g b)

instance (Functor f, Functor g) => Profunctor (Hom f g) where
  dimap :: (a' -> a) -> (b -> b') -> Hom f g a b -> Hom f g a' b'
  dimap a'2a b2b' (Hom h) = Hom $ fmap b2b' . h . fmap a'2a
```

When you pick `a` in the first source category $C^{op}$ and pick `b` in the second source category $C$, we have a profunctor from $C^{op} \times C$ to a category of morphisms from `f a` to `g b`. Since we're interested in `f c -> g c`, let's focus on cases where both `a` and `b` are the same type `c`.

What kind of morphisms of type `Hom f g c c` do we have? When we use `List` for `f` and `Maybe` for `g`, we'll have these morphisms, for example.

```
type List2Maybe a b = Hom List Maybe a b

hom1 :: List2Maybe Int Int
hom1 = Hom $ \cases
  xs | 0 `elem` xs -> Nothing
  [] -> Nothing
  (x : _) -> Just x

hom2 :: List2Maybe Int Int
hom2 = Hom $ \cases
  [] -> Nothing
  (x : _) -> Just x

hom3 :: List2Maybe Int Int
hom3 = Hom go
  where
    go [] = Nothing
    go [x] = Just x
    go (_ : xs) = go xs
```

Just like we can define cones for a functor, we can define wedges for a profunctor. To create a wedge, you first need to pick an object `x` in the target category of `f` and `g`. Then, we'll have morphisms from this object to the profunctor itself. In these Haskell examples, both source category and target category are $Hask$. So let's pick `()`, for example, as the object. Then we'll have morphisms from this object to a hom-set $Hom_{Hask}(f c, g c)$. We call it $\tau(x)$. Let's pick two candidates of such morphisms as examples.

```
tau1Int :: () -> List2Maybe Int Int
tau1Int () = hom1

tau2Int :: () -> List2Maybe Int Int
tau2Int () = hom2
```

But to be a wedge, it has to satisfy the wedge condition. The wedge condition can be written as `lmap h (tau x) == rmap h (tau x)` for any function `h :: Int -> Int` in this example. Since we picked `()` as `x`,  It's `lmap h (tau ()) == rmap h (tau ())`. Let's check if `tau1Int` and `tau2Int` satisfy this condition. We'll use `(+ 100)` as `h`.

```
l1, r1 :: Maybe Int
l1 =
  -- Just 100
  let Hom hom = lmap (+ 100) (tau1Int ())
   in hom [0, 1, 2]
r1 =
  -- Nothing
  let Hom hom = rmap (+ 100) (tau1Int ())
   in hom [0, 1, 2]

l2, r2 :: Maybe Int
l2 =
  -- Just 100
  let Hom hom = lmap (+ 100) (tau2Int ())
   in hom [0, 1, 2]
r2 =
  -- Just 100
  let Hom hom = rmap (+ 100) (tau2Int ())
   in hom [0, 1, 2]
```

As you can see, `tau1Int` doesn't satisfy this condition while `tau2Int` does. We've only seen `(+ 100)` satisfied it, but it actually satisfies it with any function. What are the differences between them? It turned out that `tua1Int` isn't a natural transformation while `tua2Int` is. I mean, `tau1Int` (`hom1`) touches the `Int` value while `tau2Int` (`hom2`) doesn't touch it. You can only use `tau1Int` with `Int`, but you can use `tau2Int` with any type. In other words, you can generalize the type of `hom2` to `List2Maybe a a` while you cannot generalize the type of `hom1`.

It turned out that the fact that a wedge satisfies the wedge condition means the hom-set pointed by the wedge is a natural transformation. Let's see some more examples. This time, we use `Bool` as well as `()` as `x`.

First, lets define two natural transformations.

```
nt2 :: List2Maybe a a
nt2 = Hom $ \cases
  [] -> Nothing
  (x : _) -> Just x

nt3 :: List2Maybe a a
nt3 = Hom go
  where
    go [] = Nothing
    go [x] = Just x
    go (_ : xs) = go xs
```

The first wedge picks `nt2`.

```
tau2 :: () -> List2Maybe a a
tau2 () = nt2
```

The second wedge picks `nt2` and `nt3`.

```
tau3 :: Bool -> List2Maybe a a
tau3 True = nt2
tau3 False = nt3
```

You can see that both of them satisfy the wedge condition.

```
l2', r2' :: Maybe Bool
l2' =
  -- Just True
  let Hom hom = lmap (== 0) (tau2 ())
   in hom [0, 1, 2]
r2' =
  -- Just True
  let Hom hom = rmap (== 0) (tau2 ())
   in hom [0, 1, 2]

l3_1', l3_2', r3_1', r3_2' :: Maybe Bool
l3_1' =
  -- Just True
  let Hom hom = lmap (== 0) (tau3 True)
   in hom [0, 1, 2]
l3_2' =
  -- Just False
  let Hom hom = lmap (== 0) (tau3 False)
   in hom [0, 1, 2]
r3_1' =
  -- Just True
  let Hom hom = rmap (== 0) (tau3 True)
   in hom [0, 1, 2]
r3_2' =
  -- Just False
  let Hom hom = rmap (== 0) (tau3 False)
   in hom [0, 1, 2]
```

Just like there might exist a limit for cones, there might exist an end for wedges. An end for wedges is the most universal wedge among them, which is a terminal object in a category of the wedges. In the above example, it's `List2Maybe a a` itself. You can think morphisms `() -> List2Maybe a a` or `Bool -> List2Maybe a a` as morphisms from `()` and `Bool` to the terminal object `List2Maybe a a` (not in $Hask$, but in the category of wedges).

An end of a profunctor $Hom_{Hask}(f a, g a)$ (`Hom f g a a`) is natural transformations from `f` to `g` (`forall a. f a -> g a`), and denoted as $\displaystyle \int_c Hask(f a, g a) \cong Nat(f, g)$.

Getting back to the original question, we can say that, when we have category $C$ and $D$, and functors $F$ and $G$ from $C$ to $D$, there is a profunctor $S(a, b)$ from $C^{op} \times C$ to $Set$, and the end of $S$ ($\displaystyle \int_c S(c, c)$) is natural transformations from $F$ to $G$.
