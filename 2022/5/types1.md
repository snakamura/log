# Expressing relations between types, part 1

Let's consider the situation where you have an item that has a category and a sub category. You can express this idea like this.

```
module Item
    ( Item
    , cat
    , subCat
    , Cat(..)
    , SubCat(..)
    , buildItem
    , makeItem
    ) where

import Control.Monad (join)
import Text.Read (readMaybe)

data Cat = Cat1 | Cat2 deriving (Show, Read, Eq)

data SubCat = SubCat1 | SubCat2 | SubCat3 deriving (Show, Read, Eq)

data Item = Item
    { cat :: Cat
    , subCat :: SubCat
    }
instance Show Item where
    show (Item cat subCat) = "Item " <> show cat <> " " <> show subCat
```

But there are some restrictions between its category and sub category. If its category is `Cat1`, its sub category must be one of `SubCat1` or `SubCat2`. If its category is `Cat2`, its sub category must be one of `SubCat2` or `SubCat3`.

To prevent someone from creating an invalid item, we can check this relation in a factory function `buildItem`, and export it from the module without exporting `Item` data constructor.

```
validSubCats :: Cat -> [SubCat]
validSubCats Cat1 = [SubCat1, SubCat2]
validSubCats Cat2 = [SubCat2, SubCat3]

buildItem :: Cat -> SubCat -> Maybe Item
buildItem cat subCat | subCat `elem` validSubCats cat = Just $ Item cat subCat
buildItem _ _ = Nothing

makeItem :: String -> String -> Maybe Item
makeItem cat subCat = join $ buildItem <$> readMaybe cat <*> readMaybe subCat
```

This works, but is it possible to put this a bit further and express this relation in types? I'll try it in this series of posts. The goal is expressing this relation in types while supporting building an item from strings and printing it.

For example, you can write something like this with the current code. How will it look like after we make it type-safe?

```
main :: IO ()
main = do
    let items :: [Item]
        items = catMaybes [ buildItem Cat1 SubCat2
                          , makeItem "Cat2" "SubCat3"
                          , makeItem "Cat2" "SubCat1"
                          ]
    mapM_ print items
```
