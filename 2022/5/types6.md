# Expressing relations between types, part 6

The last piece is to build `SCat` and `SSubCat` from a string. We can do this by introducing an existential type `SomeSCat` and `SomeSSubCat` and make them an instance of `Read`.

```
type SomeSCat :: Type
data SomeSCat where
    SomeSCat :: SCatI cat => SCat cat -> SomeSCat

instance Show SomeSSubCat where
    show (SomeSSubCat sSubCat) = show sSubCat

instance Read SomeSCat where
    readsPrec _ = readP_to_S $ choice [ string "Cat1" $> SomeSCat SCat1
                                      , string "Cat2" $> SomeSCat SCat2
                                      ]
```

```
type SomeSSubCat :: Type
data SomeSSubCat where
    SomeSSubCat :: SSubCatI subCat => SSubCat subCat -> SomeSSubCat

instance Show SomeSSubCat where
    show (SomeSSubCat sSubCat) = show sSubCat

instance Read SomeSSubCat where
    readsPrec _ = readP_to_S $ choice [ string "SubCat1" $> SomeSSubCat SSubCat1
                                      , string "SubCat2" $> SomeSSubCat SSubCat2
                                      , string "SubCat3" $> SomeSSubCat SSubCat3
                                      ]
```

With these types, you can implement `makeItem` like this.

```
makeItem :: String -> String -> Maybe SomeItem
makeItem cat subCat = do
    SomeSCat cat <- readMaybe cat
    SomeSSubCat subCat <- readMaybe subCat
    SomeItem <$> buildItem cat subCat
```

You can add `cat` and `subCat` to get `SomeSCat` and `SomeSSubCat` from `SomeItem`, with which you can get a category and a sub category at run time, just like you can do with [the original implementation](https://snak.tumblr.com/post/684284139560681472/expressing-relations-between-types-part-1).

```
cat :: SomeItem -> SomeSCat
cat (SomeItem (Item _ :: Item cat subCat)) = SomeSCat (singCat @cat)

subCat :: SomeItem -> SomeSSubCat
subCat (SomeItem (Item _ :: Item cat subCat)) = SomeSSubCat (singSubCat @subCat)
```

```
main :: IO ()
main = do
    let items :: [SomeItem]
        items = catMaybes [ SomeItem <$> buildItem SCat1 SSubCat2
                          , makeItem "Cat2" "SubCat3"
                          , makeItem "Cat2" "SubCat1"
                          ]
    mapM_ print items
```
