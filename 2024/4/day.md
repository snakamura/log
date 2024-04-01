# Data.Functor.Day

[`Data.Functor.Day`](https://hackage.haskell.org/package/kan-extensions-5.2.5/docs/Data-Functor-Day.html#t:Day) is an interesting type. It takes two functors and two values, and is a functor itself. Here is its definition.

```
data Day f g a = forall b c. Day (f b) (g c) (b -> c -> a)
```

For example, you can create values like these.

```
day1 = Day [1, 2, 3] (Just 10) (+)
day2 = Day [1, 2, 3] Nothing (+)
day3 = Day (Identity True) ("abc", 'b') (,)
```

The type of `day1` and `day2` is `Num a => Day [] Maybe a`, and the type of `day3` is `Day Maybe ((,) String) (Bool, Char)`.

What can you do when you have `day1`? Can you extract `[1, 2, 3]` or `10` from it? No, you can't. It's because `b` and `c` are existential types. For example, you cannot write a function like this.

```
extract1 :: Num a => Day [] Maybe a -> [a]
extract1 (Day fb gc bca) = fb
```

Also, you cannot write a function like this.

```
extract2 :: Day Identity ((,) String) (Bool, Char) -> Bool
extract2 (Day (Identity b) gc bca) = b
```

As you can see, even though you can always extract `b` from `Identity b`, you cannot extract it from `Day`.

Then what can you do? The only thing you can do is applying `bca :: b -> c -> a` to `b` and `c`. For example, you can apply `bca` to each element in the list with `c` in the `Maybe` if it's `Just c`.

```
doSomething :: Num a => Day [] Maybe a -> [a]
doSomething (Day fb (Just c) bca) = map (flip bca c) fb
doSomething (Day _ _ _) = []
```

`doSomething day1` will be `[11, 12, 13]`, and `doSomething day2` will be `[]`.

Note that you can, of course, extract a value once you've applied `bca`. For example, you can write this `extract2'` instead. This is because the `bca` in `day3` just puts two values into a pair. It completely makes sense to get the first element from it.

```
extract2' :: Day Identity ((,) String) (Bool, Char) -> Bool
extract2' (Day (Identity b) (_, c) bca) = fst (bca b c)
```

To sum up, you can say that you can control how you'll apply `bca` based on functors `f` and `g`, but you cannot extract `f a`, `g b`, `a` nor `b` directly. This character of `Day` becomes important when you think about natural isomorphism between `Day Identity f` and `f` where `f` is `Functor`.
