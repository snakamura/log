# Expressing relations between types, part 4

Now, it's time to build an item from strings. But before that, we'll try building an item from singletone values, `SCat` and `SSubCat`. Once we can do it, the rest of the work is just parsing these singletone values.

The first thing we need to do is defining a runtime version of `ValidSubCats`. `ValidSubCats` works on types, but this time we need a function working on values at runtime.

```
validSSubCats :: SCat cat -> HL SSubCat (ValidSubCats cat)
validSSubCats SCat1 = SSubCat1 `HCons` SSubCat2 `HCons` HNil
validSSubCats SCat2 = SSubCat2 `HCons` SSubCat3 `HCons` HNil
```

The return value of this function is a value of a heterogeneous list `HL`. `HL` isn't just a heterogenous list, but a heterogenous list of singletone values. Let's take a look at its definition.

```
type HL :: (k -> Type) -> [k] -> Type
data HL f xs where
    HCons :: f x -> HL f xs -> HL f (x ': xs)
    HNil :: HL f '[]
infixr 5 `HCons`
```

As you can see, it takes `f` which is a container of a value. In our case, it'll be `SSubCat`. We need it to write a function to check if a specified value is included in a list using propositional equality because [`TestEquality`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Type-Equality.html#t:TestEquality) works on a type indexed by a type.

```
oneOf :: TestEquality f => f x -> HL f xs -> Maybe (OneOf x xs :~: 'True)
oneOf x (HCons y ys) = case testEquality x y of
                           Just Refl -> Just Refl
                           Nothing -> unsafeCoerce $ oneOf x ys
oneOf _ _ = Nothing
```

This function tells the compiler that `OneOf x xs` must be `'True` if `y` is in `ys` when you call `oneOf y ys`.

We'll make `SSubCat` an instance of `TestEquality` to make this work.

```
instance TestEquality SSubCat where
    testEquality SSubCat1 SSubCat1 = Just Refl
    testEquality SSubCat2 SSubCat2 = Just Refl
    testEquality SSubCat3 SSubCat3 = Just Refl
    testEquality _ _ = Nothing
```

Now, when you call `oneOf sSubCat (validSSubCat sCat)` and `sSubCat` is in the list, the compiler knows that `subCat` (of type `SSubCat subCat`) is in `ValidSubCats cat` (the type of `sCat` is `SCat cat`).

With this knowledge, we can build an item from `SCat` and `SSubCat`.

```
buildItem :: SCat cat -> SSubCat subCat -> Maybe (Item cat subCat)
buildItem cat subCat = case oneOf subCat (validSSubCats cat) of
                           Just Refl -> Just Item
                           Nothing -> Nothing
```

You can use `buildItem` like this.

```
main :: IO ()
main = do
    let item1 = buildItem SCat1 SSubCat2
        item2 = buildItem SCat2 SSubCat3
        item3 = buildItem SCat2 SSubCat1
    mapM_ print $ catMaybes [ SomeItem <$> item1
                            , SomeItem <$> item2
                            , SomeItem <$> item3
                            ]
```

But wait, you'd have noticed that we use `unsafeCoerce` in `oneOf`. We needed it because we couldn't convince the compiler that `OneOf x xs` must be `'True` if `y` is in `ys` when you call `oneOf y ys`.

We'll see how we can avoid `unsafeCoerce` in the next post.
