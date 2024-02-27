# Expressing relations between types, part 3

Now, let's try making items printable. Since our `Item` is indexed by types of `Cat` and `SubCat` kinds, we cannot directly print its `cat` and `subCat`.

So first, we'll define types corresponding to these kinds so that we can get a value for each type of `Cat` and `SubCat`. These types are called singletons because it has only one value for each type.

For instance, for `Cat`, we'll define `SCat`.

```
{-# LANGUAGE DataKinds,
             GADTs,
             PolyKinds,
             ScopedTypeVariables,
             StandaloneKindSignatures,
             TypeApplications,
             TypeFamilies,
             TypeOperators
#-}

module Item
    ( Item(Item)
    , SomeItem(SomeItem)
    , type Cat(..)
    , type SubCat(..)
    ) where

import Data.Kind (Constraint, Type)

data Cat = Cat1 | Cat2

type SCat :: Cat -> Type
data SCat t where
    SCat1 :: SCat 'Cat1
    SCat2 :: SCat 'Cat2
```

As you can see, `SCat Cat1` has only one value `SCat1`, and `SCat Cat2` has only value `SCat2`. How can we associate it with `Cat`? We can use a type class to get a value from a type.

```
type SCatI :: Cat -> Constraint
class SCatI cat where
    singCat :: SCat cat
instance SCatI 'Cat1 where
    singCat = SCat1
instance SCatI 'Cat2 where
    singCat = SCat2
```

Now, you can get `SCat1` by calling `singCat @'Cat1` at runtime. Then, let's make `SCat` an instance of `Show` so that we can print it.

```
instance Show (SCat cat) where
    show SCat1 = "Cat1"
    show SCat2 = "Cat2"
```

We'll do the same thing for `SubCat`.

```
data SubCat = SubCat1 | SubCat2 | SubCat3

type SSubCat :: SubCat -> Type
data SSubCat subCat where
    SSubCat1 :: SSubCat 'SubCat1
    SSubCat2 :: SSubCat 'SubCat2
    SSubCat3 :: SSubCat 'SubCat3

instance Show (SSubCat subCat) where
    show SSubCat1 = "SubCat1"
    show SSubCat2 = "SubCat2"
    show SSubCat3 = "SubCat3"

type SSubCatI :: SubCat -> Constraint
class SSubCatI subCat where
    singSubCat :: SSubCat subCat
instance SSubCatI 'SubCat1 where
    singSubCat = SSubCat1
instance SSubCatI 'SubCat2 where
    singSubCat = SSubCat2
instance SSubCatI 'SubCat3 where
    singSubCat = SSubCat3
```

With these types and type classes, we can make `Item` an instance of `Show`.

```
type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. ToConstraint (OneOf subCat (ValidSubCats cat)) => Item cat subCat
instance (SCatI cat, SSubCatI subCat) => Show (Item cat subCat) where
    show _ = "Item " <> show (singCat @cat) <> " " <> show (singSubCat @subCat)
```

You can find `ValidSubCats`, `OneOf` and `ToConstraint` in [the previous post](https://snak.tumblr.com/post/684400816546840576/expressing-relations-between-types-part-2).

We add `SomeItem` existential type here for convenience. Then, we can put any `Item cat subCat`s to a list by wrapping `Item` in `SomeItem`.

```
data SomeItem where
    SomeItem :: (SCatI cat, SSubCatI subCat) => Item cat subCat -> SomeItem
instance Show SomeItem where
    show (SomeItem item) = show item
```

Now, we can build items, put them into a list, and print them.

```
main :: IO ()
main = do
    let item1 = Item @'Cat1 @'SubCat2
        item2 = Item @'Cat2 @'SubCat3
        -- item3 = Item @'Cat2 @'SubCat1
    print item1
    print item2
    mapM_ print [ SomeItem item1
                , SomeItem item2
                ]
```

We'll see how we can build an item from strings in the next post.
