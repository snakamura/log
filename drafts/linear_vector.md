# Using mutable vectors with linear types

You can use `ST` and [`Data.Vector.Mutable.MVector`](https://hackage-content.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Mutable.html#v:MVector) when you want to use mutable vectors.

```
import Control.Monad.ST
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

main :: Int
main = runST $ complicated 10

complicated :: Int -> ST s Int
complicated n = do
  v <- MV.new n
  fill n v
  V.sum <$> V.freeze v

fill :: Int -> MV.MVector s Int -> ST s ()
fill n v = do
  for_ [0 .. n - 1] $ \m ->
    MV.write v m (m * 42)
```

You can create a mutable vector with `MV.new`, modify it using `MV.write`, then freeze it with `V.freeze`.

The point is that you cannot take the mutable vector outside `ST s`. For example, you cannot do something like this because `s` in `ST s` is polymorphic.

```
main' :: MV.MVector s Int
main' = runST $ complicated' 10

complicated' :: Int -> ST s (MV.MVector s Int)
complicated' n = do
  v <- MV.new n
  fill n v
  pure v
```

We can use linear vectors to do similar things without `ST`.

```
import Data.Unrestricted.Linear
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear qualified as MLV
import Prelude.Linear
import Prelude ()

main :: Int
main = complicated 10

complicated :: Int -> Int
complicated n = MLV.empty $ \v ->
  case MLV.freeze $ fill n v of
    Ur v' -> V.sum v'

fill :: Int -> MLV.Vector Int %1 -> MLV.Vector Int
fill n v = foldr step v [0 .. n - 1]
  where
    step :: Int %1 -> MLV.Vector Int %1 -> MLV.Vector Int
    step m v = case move m of
      Ur m' -> MLV.push (m' * 42) v
```

You can create an empty vector using [`Data.Vector.Mutable.Linear.empty`](https://hackage-content.haskell.org/package/linear-base-0.8.1/docs/Data-Vector-Mutable-Linear.html#v:empty), but since it takes a function of type `Vector a %1 -> b` where `b` must be `Movable`, you cannot take this vector outside the function.

Use `MLV.freeze` to get `Vector` wrapped in [`Ur`](https://hackage-content.haskell.org/package/linear-base-0.8.1/docs/Data-Unrestricted-Linear.html#t:Ur) once you've finished working with the mutable vector. Since the type of data constructor `Ur` is `a -> Ur a`, you cannot put a linear value into it. It means you cannot write this `complicated'`.

```
complicated' :: Int -> Ur (MLV.Vector Int)
complicated' n = MLV.empty $ \v -> Ur (fill n v)
```

By the way, you might think that you can write `fill` like the following.

```
fill :: Int -> MLV.Vector Int %1 -> MLV.Vector Int
fill n v = foldr (\m v -> MLV.push (m * 42) v) v [0 .. n - 1]
```

But you'll find this doesn't typecheck because the type of `\m v -> MLV.push (m * 42) v` is `Int -> MLV.Vector Int %1 -> MLV.Vector Int` while the linear version of `foldr` takes `Int %1 -> MLV.Vector Int %1 -> MLV.Vector Int`.

You need to consume a linear value and get a non-linear value using [`move`](https://hackage-content.haskell.org/package/linear-base-0.8.1/docs/Data-Unrestricted-Linear.html#v:move) to pass it to `(*)` which takes two non-linear values.

Note that you cannot use `let` like this for now because GHC doesn't support analyzing linearity with `let` yet.

```
step :: Int %1 -> MLV.Vector Int %1 -> MLV.Vector Int
step m v = let Ur m' = move m
            in MLV.push (m' * 42) v
```

The code itself looks like a version using immutable vectors. Each mutation returns a new vector which is mutated in the next turn. But it's more efficient because it mutates one mutable vector internally. The linear types guarantee that each reference to the internal mutable vector is consumed once, so you don't need to worry about mutating wrong vectors, or mutating it concurrently, and so on.
