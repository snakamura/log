# T-Algebra and an adjunction

As we saw in [Initial F-Algebra of monoid and free monoid, part 1](./monoid_f_algebra_free1.html), F-Algebra of a functor `f` is a pair of a carrier type `a` and an evaluation function `alg :: f a -> a`. If `f` is also a monad, we have `pure :: a -> f a` and `join :: f (f a) -> f a`. We can say they're compatible if these two conditions hold.

- `alg . pure == id`
- `alg . join == alg . fmap alg`

The first condition means that it does nothing when you lift a value of a carrier type `a` by `pure` into the monad, then evaluate it with `alg`. The second condition means that you should get the same value when you `join` a nested monad value `f (f a)` to `f a` then evaluate it with `alg` to get `a`, and when you evaluate `f (f a)` with a lifted evaluator `fmap alg` to get `f a`, then evaluate it again with `alg` to get `a`.

Let's see an example. We'll use `Maybe` as a monad, and use `Int` as a carrier type, and use this function as an evaluator.

```
alg :: Maybe Int -> Int
alg (Just n) = n
alg Nothing = 0
```

Does this `alg` satisfy those conditions? The first condition is simple. `pure` lifts a value to `Maybe` by wrapping it in `Just`. So obviously, `alg . pure` does nothing because `alg` unwraps `Just`.

Regarding the second condition, we have three types of values of `Maybe (Maybe Int)`; `Nothing`, `Just Nothing` and `Just (Just 1)`. Let's evaluate `alg . join` and `alg . fmap alg` for these values.

```
alg (join Nothing) = 0
alg (fmap alg Nothing) = 0

alg (join (Just Nothing)) = 0
alg (fmap alg (Just Nothing)) = 0

alg (join (Just (Just 1))) = 1
alg (fmap alg (Just (Just 1))) = 1
```

You can see the conditions hold for all of these cases.

Let's take another example. We'll again use `Maybe` and `Int`, but use this `alg'` as an evaluator.

```
alg' :: Maybe Int -> Int
alg' (Just n) = -n
alg' Nothing = 0
```

Even though `alg'` is a valid F-Algebra, it's not compatible with `Maybe` monad. `alg (pure 1)` becomes `-1` so `alg . pure` isn't `id`.

An F-algebra that is compatible with a monad is called T-Algebra or Eilenburg-Moore algebra. In the examples above, a pair `(Int, alg)` is a T-Algebra for `Maybe`, but `(Int, alg')` is not.

Let's take yet another example. In this example, we'll use `Maybe` as a monad, `Maybe Int` as a carrier type, and `join` as an evaluator. Since the type of `join` is `Maybe (Maybe Int) -> Maybe Int`, you can use it as an evaluator of this F-Algebra.

We have two types of values of the carrier type `Maybe Int`; `Nothing` and `Just 1`, for instance. You can see that these two values satisfy the first condition.

```
join (pure Nothing) = Nothing
join (pure (Just 1)) = Just 1
```

There are also these four types of values of `Maybe (Maybe (Maybe Int))`; `Nothing`, `Just Nothing`, `Just (Just Nothing)` and `Just (Just (Just 1))`. These values satisfy the second condition.

```
join (join Nothing) = Nothing
join (fmap join Nothing) = Nothing

join (join (Just Nothing)) = Nothing
join (fmap join (Just Nothing)) = Nothing

join (join (Just (Just Nothing))) = Nothing
join (fmap join (Just (Just Nothing))) = Nothing

join (join (Just (Just (Just 1)))) = Just 1
join (fmap join (Just (Just (Just 1)))) = Just 1
```

So a pair `(Maybe Int, join)` is a T-algebra for `Maybe`, and is called a free T-algebra. But you can see that these conditions hold not only with `Int`, but with any type `a`. We can say that `(Maybe a, join)` is a T-algebra. Actually, they hold for any monad `f`, and we can say that `(f a, join)` is a free T-algebra for any monad `f`.

Now, let's think about a category of T-algebras for `Maybe`, and call it $Hask^{Maybe}$. In this category, an object is a pair of a carrier type `a` and an algebra `alg :: Maybe a -> a`.

There is a functor $F^{Maybe}$ from $Hask$ to $Hask^{Maybe}$. This functor maps an object in $Hask$ (type `a`) to a free T-algebra in $Hask^{Maybe}$ (`(Maybe a, join)`). It maps a morphism in $Hask$ (a function `a -> b`) to a morphism in $Hask^{Maybe}$ (`(Maybe a, join) -> (Maybe b, join)`). It can map a morphism this way because `join` is polymorphic. I mean, its type is `forall a. (Maybe (Maybe a) -> Maybe a)`, so it should work for both `a` and `b`.

Also, there is a forgetful functor $U^{Maybe}$ from $Hask^{Maybe}$ to $Hask$ which maps an object `(a, alg :: Maybe a -> a)` to `a`, and maps a morphism in $Hask^{Maybe}$ (`(a, alg) -> (b, alg')`) to a morphism in $Hask$ (`a -> b`).

It turns out that $F^{Maybe}$ is a left adjoint and $U^{Maybe}$ is a right adjoint. Let's check what will be `unit` and `counit` of this adjunction.

`unit` is a natural transformation $I \to U^{Maybe} \circ F^{Maybe}$ where `I` is an identify functor. When you pick an object in $Hask$ `a`, and apply $F^{Maybe}$, you'll get `(Maybe a, join)`, then applying $U^{Maybe}$ to get `Maybe a`. So we need a morphism `a -> Maybe a` for `unit`, and we can use `pure`.

`counit` is a natural transformation $F^{Maybe} \circ U^{Maybe} \to I$. When you pick an object in $Hask^{Maybe}$ `(a, alg)`, you'll get `a` by applying $U^{Maybe}$ to it, then applying $F^{Maybe}$ to get `(Maybe a, join)`. So we need a homomorphism `(Maybe a, join) -> (a, alg)` for `counit`, and we can use `alg` itself as this homomorphism. You can see it in this diagram.

<pre><code>                fmap alg
<strong>Maybe (Maybe a)</strong>    →→→   <strong>Maybe a</strong>
      ↓                     ↓
 join ↓                     ↓ alg
      ↓                     ↓
   <strong>Maybe a</strong>         →→→      <strong>a</strong>
                   alg
</code></pre>

This adjunction gives rise to `Maybe` monad itself. When you have `a`, applying $F^{Maybe}$ creates a pair `(Maybe a, join)`, and applying $U^{Maybe}$ drops the evaluator `join`, and you'll get `Maybe a`. So $U^{Maybe} \circ F^{Maybe} == Maybe$.

This applies to any monad `T`. $F^T$ is a left adjoint and $U^T$ is a right adjoint, and the composition $U^T \circ F^T$ gives rise to the monad `T` itself.
