# Initial F-Algebra of monoid and free monoid, part 1

Monoid has two operations `mappend` and `mempty` that satisfy some conditions such as associativity law and unit law. When you ignore these conditions, you can express monoid as a combination of these two operations.

```
data Monoid a = Append a a
              | Empty

instance Functor Monoid where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty
```

You can have functions of type `Monoid a -> a` to evaluate this monoid. For example, `sumIntAlg` evaluates it with additions, and `productIntAlg` evaluates it with multiplications.

```
sumIntAlg :: Monoid Int -> Int
sumIntAlg (Append m n) = m + n
sumIntAlg Empty = 0

productIntAlg :: Monoid Int -> Int
productIntAlg (Append m n) = m * n
productIntAlg Empty = 1
```

Let's evaluate some values with them.

```
v1, v2 :: Int
v1 = sumIntAlg (Append 1 2)
v2 = productIntAlg (Append 1 2)
```

A set of `Monoid`, `Int` and a function of type `Monoid Int -> Int` is called F-Algebra.

`sumIntAlg` and `productIntAlg` aren't that interesting, but we can build `Monoid` on `Monoid Int` as well.

```
sumMonoidIntAlg :: Monoid (Monoid Int) -> Monoid Int
sumMonoidIntAlg (Append m n) = Append (sumIntAlg m) (sumIntAlg n)
sumMonoidIntAlg Empty = Empty
```

We can now evaluate a monoid with depth-one trees with `sumMonoidIntAlg`.

```
v3 :: Int
v3 = sumIntAlg $ sumMonoidIntAlg $ Append (Append 1 2) Empty
```

We can repeat it to evaluate a monoid with depth-two trees and so on.

```
sumMonoidMonoidIntAlg :: Monoid (Monoid (Monoid Int)) -> Monoid (Monoid Int)
sumMonoidMonoidIntAlg (Append m n) = Append (sumMonoidIntAlg m) (sumMonoidIntAlg n)
sumMonoidMonoidIntAlg Empty = Empty

v4 :: Int
v4 = sumIntAlg $ sumMonoidIntAlg $ sumMonoidMonoidIntAlg $ Append (Append (Append 1 2) (Append 3 4)) Empty
```

When you repeat this process infinite times, you'll get a point where repeating it one more time doesn't change its structure. We can express it using `Fix` below.

```
newtype Fix f = In (f (Fix f))

out :: Fix f -> f (Fix f)
out (In f) = f
```

When you have a value `In Empty :: Fix Monoid`, applying `In` one more time like `In (Append (In Empty) (In Empty))` won't change its type. It's still `Fix Monoid`.

An evaluator of `Fix Monoid` is `In`. It takes `Monoid (Fix Monoid)` and returns `Fix Monoid`. A set of `Monoid`, `Fix Monoid` and `In` is called an initial F-Algebra. This is because it's an initial object of a category of F-Algebra where an object is a pair of `a` and `Monoid a -> a`. Since a pair of `Fix Monoid` and `In` is an initial object of this category, there should be a unique morphism from it to any other objects.

This morphism is a homomorphism because it preserves a structure of F-Algebra. Let's take `Monoid Int` and `sumIntAlg` as an example.

<pre><code>                    fmap homInitialToSumInt
<strong>Monoid (Fix Monoid)</strong>           →→→           <strong>Monoid Int</strong>
         ↑ ↓                                     ↓
     out ↑ ↓ In                                  ↓ sumIntAlg
         ↑ ↓                                     ↓
     <strong>Fix Monoid</strong>               →→→               <strong>Int</strong>
                      homInitialToSumInt
</code></pre>

<!--
```mermaid
block-beta
columns 1
  block:TOP
    A["Monoid (Fix Monoid)"]
    space:2
    B["Monoid Int"]
  end
  space
  block:BOTTOM
    C["Fix Monoid"]
    space:2
    D["Int"]
  end
  A -- "fmap homInitialToSumInt" -- > B
  A -- "In" -- > C
  C -. "out" .-> A
  C -- "homInitialToSumInt" -- > D
  B -- "sumIntAlg" -- > D
  style TOP fill:transparent,stroke:none
  style BOTTOM fill:transparent,stroke:none
```
-->

We'll define a morphism `homInitialToSumInt` from `Fix Monoid` to `Int`. First, we'll apply `out` to get `Monoid (Fix Monoid)`. Then, apply `fmap homInitialToSumInt` itself to get `Monoid Int`. Finally, we'll apply `sumIntAlg` to get `Int`.

```
homInitialToSumInt :: Fix Monoid -> Int
homInitialToSumInt = sumIntAlg . fmap homInitialToSumInt . out
```

`Fix Monoid` has an infinite nested `Monoid a`, and you can think `homInitialToSumInt` applies itself to the next level recursively using `fmap`.

What monoids can we have with these definitions? For instance, we can have `Fix Monoid` like these.

```
m0, m1 :: Fix Monoid
m0 = In Empty
m1 = In (Append (In Empty) (In (Append (In Empty) (In Empty))))
```

You can apply `homInitialToSumInt` to evaluate them.

```
i0, i1 :: Int
i0 = homInitialToSumInt m0
i1 = homInitialToSumInt m1
```

Of course, `i0` and `i1` will evaluate to `0`. This isn't very interesting. We'll see how we can bring actual `Int` values into this in the next post.

By the way, `homInitialToSumInt` can be generalized to any F-Algebra of type `Monoid Int -> Int` like this.

```
homInitialToInt :: (Monoid Int -> Int) -> Fix Monoid -> Int
homInitialToInt alg = alg . fmap (homInitialToInt alg) . out
```

Then, you can generalize to any F-Algebra of type `Monoid a -> a`.

```
hom :: (Monoid a -> a) -> Fix Monoid -> a
hom alg = alg . fmap (hom alg) . out
```

Finally, this can be generalized to any functor `f`, and it's called a catamorphism.

```
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out
```
