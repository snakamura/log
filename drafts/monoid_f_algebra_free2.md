# Initial F-Algebra of monoid and free monoid, part 2

In [the previous post](./monoid_f_algebra_free1.html), we saw an F-Algebra of monoid, but we could only create a monoid structure with monoid units (`Empty`) because `Monoid` in the post only represents a structure of monoid. What can we do to have actual values in it?

We need one more type parameter representing a type of a value, and one more data constructor to lift a value into a monoid structure.

```
data FreeMonoid a r = Append r r
                    | Empty
                    | Lift a

instance Functor (FreeMonoid a) where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty
    fmap _ (Lift n) = Lift n
```

`FreeMonoid` has `a` in addition to `r` (which is equivalent to `a` in the previous post) as its type parameters. It also has `Lift` to lift a value.

`sumIntAlg` and `productIntAlg` are almost identical to the ones in the previous post, but they handle `Lift`. As you can see, these algebras return the lifted value itself.

```
sumIntAlg :: FreeMonoid Int Int -> Int
sumIntAlg (Append m n) = m + n
sumIntAlg Empty = 0
sumIntAlg (Lift n) = n

productIntAlg :: FreeMonoid Int Int -> Int
productIntAlg (Append m n) = m * n
productIntAlg Empty = 1
productIntAlg (Lift n) = n
```

We can do the same with `sumMonoidIntAlg` and `sumMonoidMonoidIntAlg`.

```
sumMonoidIntAlg :: FreeMonoid Int (FreeMonoid Int Int) -> FreeMonoid Int Int
sumMonoidIntAlg (Append m n) = Append (sumIntAlg m) (sumIntAlg n)
sumMonoidIntAlg Empty = Empty
sumMonoidIntAlg (Lift n) = Lift n

sumMonoidMonoidIntAlg :: FreeMonoid Int (FreeMonoid Int (FreeMonoid Int Int)) -> FreeMonoid Int (FreeMonoid Int Int)
sumMonoidMonoidIntAlg (Append m n) = Append (sumMonoidIntAlg m) (sumMonoidIntAlg n)
sumMonoidMonoidIntAlg Empty = Empty
sumMonoidMonoidIntAlg (Lift n) = Lift n
```

Using the same `Fix` as in the previous post, you can define values of `Fix (FreeMonoid Int)` with values.

```
m0, m1, m2, m3 :: Fix (FreeMonoid Int)
m0 = In Empty
m1 = In (Lift 1)
m2 = In (Append (In (Append (In (Lift 1)) (In (Lift 2)))) (In Empty))
m3 = In (Append (In (Append (In (Lift 1)) (In (Append (In (Lift 2)) (In (Lift 3)))))) (In Empty))
```

These values are more interesting than `m0`, `m1` in the previous post because they have values other than `Empty`. You can evaluate them to `Int` using `cata` from the previous post.

```
i0, i1, i2, i3 :: Int
i0, i1, i2 :: Int
i0 = cata sumIntAlg m0
i1 = cata sumIntAlg m1
i2 = cata sumIntAlg m2
i3 = cata sumIntAlg m3
```

`Fix (FreeMonoid Int)` is a free monoid of `Int`. You can build a monoidal structure without evaluating it at that point, and lazily evaluate it with any algebra later. We usually call a list a free monoid, but `Fix (FreeMonoid Int)` can be flattened to a list using the associativity of a monoid. For example, `m3` represents `(1 + (2 + 3)) + 0`, but it's equivalent to `1 + (2 + (3 + 0))` which can be represented by a list.

It's interesting that both the initial F-Algebra of monoid and the free construction of monoid result in the same free monoid.
