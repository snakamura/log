# Terminal F-Coalgebra of monoid and cofree monoid

We saw F-Algebra of monoid in [this post](./monoid_f_algebra_free1.html). Then, what's the dual of it? It's a F-Coalgebra of monoid. Let's see what it is.

We will use the same `Monoid` definition in this post.

```
data Monoid a = Append a a
              | Empty
  deriving Show

instance Functor Monoid where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty
```

F-Algebra was a pair of a carrier type `a` and an evaluation function of type `Monoid a -> a`. F-Coalgebra is a pair of a carrier type `a` and a function of type `a -> Monoid a` where an arrow is reversed.

For example, we can define such coalgebra like this.

```
sumIntCoalg :: Int -> Monoid Int
sumIntCoalg 0 = Empty
sumIntCoalg n = let m = n `div` 2 in Append m (n - m)
```

This coalgebra builds a monoid by dividing a number so that their sum becomes the number. You can build a monoid like this with it.

```
v1, v2 :: Monoid Int
v1 = sumIntCoalg 0
v2 = sumIntCoalg 3
```

`v1` will be `Empty` and `v2` will be `Append 1 2`.

Just like we did for the F-Algebra, let's see what happens when we use `Monoid Int` as a carrier type.

```
sumMonoidIntCoalg :: Monoid Int -> Monoid (Monoid Int)
sumMonoidIntCoalg Empty = Empty
sumMonoidIntCoalg (Append m n) = Append (sumIntCoalg m) (sumIntCoalg n)
```

`sumMonoidIntCoalg` splits each value in `Append` into two by applying `sumIntCoalg`.

```
v3 :: Monoid (Monoid Int)
v3 = sumMonoidIntCoalg $ sumIntCoalg 3
```

`v3` will be `Append (Append 0 1) (Append 1 1)`.

Let's repeat this one more time and use `Monoid (Monoid Int)` as a carrier type.

```
sumMonoidMonoidIntCoalg :: Monoid (Monoid Int) -> Monoid (Monoid (Monoid Int))
sumMonoidMonoidIntCoalg Empty = Empty
sumMonoidMonoidIntCoalg (Append m n) = Append (sumMonoidIntCoalg m) (sumMonoidIntCoalg n)
```

`sumMonoidMonoidIntCoalg` again splits each value in `Append` into two.

```
v4 :: Monoid (Monoid (Monoid Int))
v4 = sumMonoidMonoidIntCoalg $ sumMonoidIntCoalg $ sumIntCoalg 5
```

`v4` will be `Append (Append Empty (Append 0 1)) (Append (Append 0 1) (Append 0 1))`. This time, it'll generate `Empty` because the first parameter of `Append` is `0`.

When you repeat this process infinite times, you'll get a point where repeating it one more time doesn't change its structure. We can express it using `Fix` again.

```
newtype Fix f = In (f (Fix f))

out :: Fix f -> f (Fix f)
out (In f) = f
```

In the category of F-Algebra, a pair of `Fix Monoid` and `In` was an initial object. In the category of F-Coalgebra, a pair of `Fix Monoid` and `out` is a terminal object. There should be a unique morphism from any other objects to it.

Let's take `Monoid Int` and `sumIntCoalg` as an example.

<pre><code>                    fmap homSumIntToTerminal
<strong>Monoid (Fix Monoid)</strong>            ←←←           <strong>Monoid Int</strong>
         ↑ ↓                                     ↑
     out ↑ ↓ In                                  ↑ sumIntCoalg
         ↑ ↓                                     ↑
     <strong>Fix Monoid</strong>                ←←←              <strong>Int</strong>
                      homSumIntToTerminal
</code></pre>

We can define a homomorphism `homSumIntToTerminal` from `Int` to `Fix Monoid` by first applying `sumIntCoalgo`, then `fmap homSumIntToTerminal`, and finally `In`.

```
homSumIntToTerminal :: Int -> Fix Monoid
homSumIntToTerminal = In . fmap homSumIntToTerminal . sumIntCoalg
```

You can apply `Int` to `homSumIntToTerminal` to get a terminal coalgebra.

```
m0, m1 :: Fix Monoid
m0 = homSumIntToTerminal 0
m1 = homSumIntToTerminal 3
```

`m0` becomes `Fix Empty`, but `m1` becomes `Append` containing an infinite number of `Empty` and `Append` inside.

Just like an initial F-Algebra defines a free structure, a terminal F-Coalgebra defines a cofree structure which is a maximum structure of the monoid. For example, when you have `3`, we can expand it to an infinite size of monoid like `(0 <> (0 <> (... <> 1))) <> ((0 <> (... <> 1)) <> (0 <> (... <> 1)))` by prepending its unit (`0`) to each non-unit value.

By the way, `homSumIntToTerminal` can be generalized to any F-Coalgebra of type `Int -> Monoid Int` like this.

```
homIntToTerminal :: (Int -> Monoid Int) -> Int -> Fix Monoid
homIntToTerminal coalg = In . fmap (homIntToTerminal coalg) . coalg
```

It can be further generalized to any F-Coalgebra of type `a -> Monoid a`.

```
hom :: (a -> Monoid a) -> a -> Fix Monoid
hom coalg = In . fmap (hom coalg) . coalg
```

Finally, it can be generalized to any functor `f`, and it's called an anamorphism.

```
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg
```
