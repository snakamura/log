# Natural isomorphism between `Day Identity f` and `f`

In [the previous post](./day.html), we saw what we can do with `Data.Functor.Day`. Now, let' see natural isomorphism between `Day Identity f` and `f` where `f` is a functor.

In Haskell, you can say type `a` and type `b` are isomorphic when you can write a function `f :: a -> b` and `g :: b -> a` where `f . g = id` and `g .f = id`.

For example, when you have

```
data Item idType nameType valueType = Item idType nameType valueType
```

`Item idType nameType valueType` is isomorphic to `(idType, (nameType, valueType))` where

```
f :: Item idType nameType valueType -> (idType, (nameType, valueType))
f (Item id name value) = (id, (name, value))

g :: (idType, (nameType, valueType)) -> Item idType nameType valueType
g (id, (name, value)) = Item id name value
```

The same goes for natural isomorphism. You can say a functor `f` and `g` are naturally isomorphic when you can write a function `p :: f a -> g a` and `q :: g a -> f a` for any `a` where `p . q = id` and `q . p = id`.

For example, when you have

```
data Option a = Some a | None
```

`Option` (not `Option a`) is isomorphic to `Maybe` where

```
p :: Option a -> Maybe a
p (Some a) = Just a
p None = Nothing

q :: Maybe a -> Option a
q (Just a) = Some a
q Nothing = None
```

When you introduce `~>` type to express natural transformations like this,

```
type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall a. f a -> g a
```

you can write types of `p` and `q` like these.

```
p :: Option ~> Maybe
q :: Maybe ~> Option
```

`p` and `q` look more like `f` and `g` above now.

So now, I'm wondering whether I can say `Day Identity f` and `f` are naturally isomorphic. To say that, we need `p :: Day Identity f ~> f` and `q :: f ~> Day Identity f` where `p . q = id` and `q . p = id`. Let's implement them.

```
p :: Functor f => Day Identity f ~> f
p (Day (Identity b) gc bca) = fmap (bca b) gc

q :: Functor f => f ~> Day Identity f
q fa = Day (Identity ()) fa (flip const)
```

Can we say `p . q = id` and `q . p = id`?

For example, when you have `Day (Identity 1) (Just 3) (+)` and pass it to `p`, it'll be `Just 4`. Then, passing `Just 4` to `q` will be `Day (Identity ()) (Just 4) (flip const)`. They don't seem identical.

But as we saw in [the previous post](./day.html), all we can do with these values is applying `bca` to `b` and `c`. When you pick `Day (Identity 1) (Just 3) (+)` and apply `(+)` to `1` and `3`, you'll get `4`. Also, when you pick `Day (Identity ()) (Just 4) (flip const)` and apply `flip const` to `()` and `4`, you'll get `4`. So they're identical in this sense.

The opposite is simpler. When you apply `q` to `Just 4`, you'll get `Day (Identity ()) (Just 4) (flip const)`. When you apply `q` to it, you'll get `Just 4`. So they're identical.

The same goes for `Day (Identity 1) Nothing (+)`. Applying `p` transforms it to `Nothing`, and applying `q` results in `Day (Identity ()) Nothing (flip const)`. All we can get from both of them is `Nothing` even though the latter no longer has `1` in it.

Let's take one more example with `Day (Identity True) ("abc", 'b') (,)`. When you pass it to `p`, you'll get `("abc", (True, 'b'))`. When you pass it to `q`, you'll get `Day (Identity ()) ("abc", (True, 'b')) (flip const)`. Even though they don't look the same, all we can get from both of them is `("abc", (True, 'b'))`.

Since `Identity` cannot add any additional computations, all we can do with `Day Identity f a` is `fmap`ing `bca` in `f`, and getting `a` no matter whether `a` is from `b` or `c`. It seems that we can say that `Day Identity f` and `f` are naturally isomorphic. This becomes important when you think about a monoidal category consists of a category of endofunctors, `Day` as a tensor product, and `Identity` as its unit.
