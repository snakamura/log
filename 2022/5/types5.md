# Expressing relations between types, part 5

The problem was that we couldn't write `oneOf` without using `unsafeCoerce`. This is because the return type (kind) of `OneOf` is `Bool`.

```
type OneOf :: k -> [k] -> Bool
type family OneOf t ts where
    OneOf t (t ': _) = 'True
    OneOf t (_ ': ts) = OneOf t ts
    OneOf _ _ = 'False
```

The compiler cannot be convinced that `OneOf t (_ ': ts)` is `'True` when `OneOf t ts` is `'True`.

If the problem is that it returns `Bool`, what type should we return? We need a type from which a compiler can see where a specific type is included.

```
type Elem :: k -> [k] -> Type
data Elem t ts where
    Here :: Elem t (t ': ts)
    There :: Elem t ts -> Elem t (t' ': ts)
```

If a value is `Here`, it means that the type is at the beginning of the list of types. If the value is `There Here`, the type is the second element in the list. If `There (There Here)`, it's the third element, and so on.

We'll modify the definition of `Item` to use `Elem` instead of `OneOf`.

```
type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. Elem subCat (ValidSubCats cat) -> Item cat subCat
```

Instead of having `OneOf` constraint, we now need to pass a value of `Elem` explicitly when creating a value of `Item`. For example, something like this.

```
item1 = Item @'Cat1 @'SubCat1 Here
item2 = Item @'Cat1 @'SubCat2 (There Here)
```

Then, instead of `oneOf`, we define `hElem` which returns `Maybe Elem`. This function returns `Just Here`, `Just (There Here)`, `Just (There (There Here))` and so on if the specified singletone value is included in the list of singleton values, and return `Nothing` otherwise.

```
hElem :: TestEquality f => f x -> HL f xs -> Maybe (Elem x xs)
hElem x (HCons y ys) = case testEquality x y of
                           Just Refl -> Just Here
                           Nothing -> There <$> hElem x ys
hElem _ _ = Nothing
```

As you can see, we no longer need `unsafeCoerce` here.

With these changes, we can now implement `buildItem`.

```
buildItem :: SCat cat -> SSubCat subCat -> Maybe (Item cat subCat)
buildItem cat subCat = Item <$> hElem subCat (validSSubCats cat)
```

This `buildItem` has the same type as the `buildItem` in the previous post, so you can use it in the same way.
