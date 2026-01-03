# Dinatural transformations

When you have two functors, there may be morphisms between them in a category of functors, and some of them might be natural transformations. For morphisms to be natural transformations, they must satisfy

$$fmap \, a2b \circ nat_a \cong nat_b \circ fmap \, a2b$$

for any $a2b :: a \rightarrow b$. For example, let's take `Maybe` and `List` as an example.

```
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []
```

For `maybeToList` to be a natural transformation, `fmap a2b . maybeToList == maybeToList . fmap a2b`. Let's compare both sides. You can see both sides become the same value.

```
   (fmap a2b . maybeToList) Nothing
-> fmap a2b [] $ maybeToList Nothing
-> fmap a2b []
-> []

   (fmap a2b . maybeToList) (Just a)
-> fmap a2b [a] $ maybeToList (Just a)
-> fmap a2b [a]
-> [a2b a]
```

```
   (maybeToList . fmap a2b) Nothing
-> maybeToList $ fmap a2b Nothing
-> maybeToList Nothing
-> []

   (maybeToList . fmap a2b) (Just a)
-> maybeToList $ fmap a2b (Just a)
-> maybeToList (Just (a2b a))
-> [a2b a]
```

In the same token, there may be morphisms between two profunctors, and some of them might be natural transformations. For morphisms to be natural transformations, they must satisfy

$$
\begin{align*}
  &\, rmap \, a2b \circ nat_{aa} \circ lmap \, a2b \\
  \cong &\, lmap \, a2b \circ nat_{bb} \circ rmap \, a2b \\
  \cong &\, rmap \, a2b \circ lmap \, a2b \circ nat_{ba} \\
  \cong &\, lmap \, a2b \circ rmap \, a2b \circ nat_{ba}
\end{align*}
$$

for any $a2b :: a \rightarrow b$. Notice that all of them transform $p \, b \, a$ to $q \, a \, b$.

Natural transformations that satisfy the first isomorphisms are called dinatural transformations.

$$rmap \, a2b \circ nat_{a} \circ lmap \, a2b \cong lmap \, a2b \circ nat_{b} \circ rmap \, a2b$$

While natural transformations transform a profunctor $p \, b \, a$ to a profunctor $q \, b \, a$, dinatural transformations transform only a profunctor $p \, a \, a$ to a profunctor $q \, a \, a$. They only care about diagonal elements. That's why they're indexed only by one object such as $nat_{a}$ instead of two ($nat_{aa}$).

For example, when you have two profunctors `Pure` and `Star`, you can have a dinatural transformation `pureStar` from the former to the latter.

```
newtype Pure a b = Pure (a -> b)

instance Profunctor Pure where
  dimap :: (s -> a) -> (b -> t) -> Pure a b -> Pure s t
  dimap s2a b2t (Pure f) = Pure (b2t . f . s2a)

newtype Star f a b = Star (a -> f b)

instance (Functor f) => Profunctor (Star f) where
  dimap :: (s -> a) -> (b -> t) -> (Star f a b -> Star f s t)
  dimap s2a b2t (Star a2fb) =
    Star
      ( \s ->
          let a = s2a s
              fb = a2fb a
              ft = fmap b2t fb
           in ft
      )
```

```
pureStar :: forall f a. (Applicative f) => Pure a a -> Star f a a
pureStar (Pure a2a) = Star a2fa
  where
    a2fa :: a -> f a
    a2fa = pure . a2a
```

Let's expand both sides of the dinaturality condition to see if `pureStar` satisfies the dinaturality condition.

```
   (rmap a2b . pureStar . lmap a2b) (Pure b2a)
-> rmap a2b $ pureStar $ lmap a2b $ Pure b2a
-> rmap a2b $ pureStar $ Pure (b2a . a2b)
-> rmap a2b $ Star (pure . b2a . a2b)
-> Star (fmap a2b . pure . b2a . a2b)
-> Star (pure . a2b . b2a . a2b) -- fmap a2b . pure = pure . a2b
```

```
   (lmap a2b . pureStar . rmap a2b) (Pure b2a)
-> lmap a2b $ pureStar $ rmap a2b $ Pure b2a
-> lmap a2b $ pureStar $ Pure (a2b . b2a)
-> lmap a2b $ Star (pure . a2b . b2a)
-> Star (pure . a2b . b2a . a2b)
```

Another example is a dinatural transformation between two `Star`s using a natural transformation between two functors `f` and `g`.

```
hoistStar :: (forall x. f x -> g x) -> Star f a a -> Star g a a
hoistStar fx2gx (Star a2fa) = Star (fx2gx . a2fa)
```

You can see `hoistStar fx2gx` satisfies the dinaturality condition, too.

```
   (rmap a2b . hoistStar fx2gx . lmap a2b) (Star b2fa)
-> rmap a2b $ hoistStar fx2gx $ lmap a2b $ Star b2fa
-> rmap a2b $ hoistStar fx2gx $ Star (b2fa . a2b)
-> rmap a2b $ Star (fx2gx . b2fa . a2b)
-> Star (fmap a2b . fx2gx . b2fa . a2b)
```

```
   (lmap a2b . hoistStar fx2gx . rmap a2b) (Star b2fa)
-> lmap a2b $ hoistStar fx2gx $ rmap a2b $ Star b2fa
-> lmap a2b $ hoistStar fx2gx $ Star (fmap a2b . b2fa)
-> lmap a2b $ Star (fx2gx . fmap a2b . b2fa)
-> Star (fx2gx . fmap a2b . b2fa . a2b)
-> Star (fmap a2b . fx2gx . b2fa . a2b) -- fx2gx . fmap a2b = fmap a2b . fx2gx
```

Actually, they satisfy the naturality condition as well when you relax their types. So they're natural transformations. Notice that a natural transformation `pureStar` is indexed by `a` and `b` while the previous `pureStar` was index only by `a`.

```
pureStar :: forall f a b. (Applicative f) => Pure a b -> Star f a b
pureStar (Pure a2b) = Star a2fb
  where
    a2fb :: a -> f b
    a2fb = pure . a2b
```

Let's expand the naturailty condition for this `pureStar` to see if it satisfies the naturality conditions in addition to the dinaturality condition.

```
   (rmap a2b . pureStar . lmap a2b) (Pure b2a)
-> rmap a2b $ pureStar $ lmap a2b $ Pure b2a
-> rmap a2b $ pureStar $ Pure (b2a . a2b)
-> rmap a2b $ Star (pure . b2a . a2b)
-> Star (fmap a2b . pure . b2a . a2b)
-> Star (pure . a2b . b2a . a2b) -- fmap a2b . pure = pure . a2b
```

```
   (lmap a2b . pureStar . rmap a2b) (Pure b2a)
-> lmap a2b $ pureStar $ rmap a2b $ Pure b2a
-> lmap a2b $ pureStar $ Pure (a2b . b2a)
-> lmap a2b $ Star (pure . a2b . b2a)
-> Star (pure . a2b . b2a . a2b)
```

```
   (rmap a2b . lmap a2b . pureStar) (Pure b2a)
-> rmap a2b $ lmap a2b $ Star (pure . b2a)
-> rmap a2b $ Star (pure . b2a . a2b)
-> Star (fmap a2b . pure . b2a . a2b)
-> Star (pure . a2b . b2a . a2b) -- fmap a2b . pure = pure . a2b
```

```
   (lmap a2b . rmap a2b . pureStar) (Pure b2a)
-> lmap a2b $ rmap a2b $ Star (pure . b2a)
-> lmap a2b $ Star (fmap a2b . pure . b2a)
-> Star (fmap a2b . pure . b2a . a2b)
-> Star (pure . a2b . b2a . a2b) -- fmap a2b . pure = pure . a2b
```

Are there any dinatural transformations which aren't natural transformations? Yes, for example, there is a dinatural transformation from a constant profunctor to a pure profunctor.

```
newtype Const r a b = Const r

instance Profunctor (Const r) where
  dimap :: (s -> a) -> (b -> t) -> Const r a b -> Const r s t
  dimap _ _ (Const r) = Const r
```

`constPure` is a dinatural transformation from `Const r a a` to `Pure a a`.

```
constPure :: Const r a a -> Pure a a
constPure _ = Pure id
```

But it's not a natural transformation. You should be able to get `b` from `a` when you want to make it a natural transformation, but it's not always possible, and cannot satisfy the naturality condition.

```
constPure :: Const r a b -> Pure a b
constPure _ = Pure ???
```
