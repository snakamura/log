# Expressing relations between types, part 2

Let's first forget about building an item from strings and printing, but focus on the definition of `Item`. We'll pass two types to the type constructor of `Item`, instead of passing two values to the data constructor of `Item` in the previous post.

To implement this we need kinds `Cat` and `SubCat`. This time we use them as kinds not as types, and use `Cat1`, `Cat2`, `SubCat1`, `SubCat2`, `SubCat3` as types not as values.

```
{-# LANGUAGE DataKinds,
             ExplicitForAll,
             GADTs,
             PolyKinds,
             StandaloneKindSignatures,
             TypeFamilies,
             TypeOperators
#-}

module Item
    ( Item(Item)
    , type Cat(..)
    , type SubCat(..)
    ) where

import Data.Kind (Constraint, Type)

data Cat = Cat1 | Cat2
data SubCat = SubCat1 | SubCat2 | SubCat3
```

Now, let's define the relationship between them. We can define a type family to express the relationship.

```
type ValidSubCats :: Cat -> [SubCat]
type family ValidSubCats cat where
    ValidSubCats 'Cat1 = '[ 'SubCat1, 'SubCat2 ]
    ValidSubCats 'Cat2 = '[ 'SubCat2, 'SubCat3 ]
```

You can think `ValidSubCats` as a type level function that returns a list of valid `SubCat`s from a `Cat`. With this definition, you can write `Item` like this.

```
type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. ToConstraint (OneOf subCat (ValidSubCats cat)) => Item cat subCat
```

The constriant part (`ToConstraint (OneOf subCat (ValidSubCats cat))`) expresses the idea that `subCat` passed to `Item` must be one of the valid `SubCat`s for a specified `cat`. `OneOf` is another type level function that returns `'True` if a specified type is in the list of types and `'False` otherwise.

```
type OneOf :: k -> [k] -> Bool
type family OneOf t ts where
    OneOf t (t ': _) = 'True
    OneOf t (_ ': ts) = OneOf t ts
    OneOf _ _ = 'False
```

We'll convert a result to a constraint using another type level function to use it in the constraint part.

```
type ToConstraint :: Bool -> Constraint
type family ToConstraint b where
    ToConstraint 'True = ()
    ToConstraint 'False = ('True ~ 'False)
```

We now can make it a compile error when we accidentally create an item with invalid combination of `Cat` and `SubCat`. But, for now, what we can do is just creating valid items like this. We cannot even print them. It's not that useful, but we'll add more based on this in the next few posts.

```
main :: IO ()
main = do
    let -- Create an instance using TypeApplications
        item1 = Item @'Cat1 @'SubCat2
        -- Create an instance by specifying its type
        item2 = Item :: Item Cat2 SubCat3
        -- A compiler will make this an error
        -- item3 = Item @'Cat2 @'SubCat1
    print "Item"
```
