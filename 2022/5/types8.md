# Expressing relations between types, part 8

So how could it be shorter with a true dependent type language like Idris?

It's good that we no longer need singleton types and convert types and those singleton types and vise versa, but we still need to write a lot even though instances of `DecEq` and `Show` might be able to be derived using elaborator reflection.

```
import Data.List.Elem
import Decidable.Equality.Core

data Cat = Cat1 | Cat2

cat1NotCat2 : Cat1 = Cat2 -> Void
cat1NotCat2 Refl impossible

DecEq Cat where
    decEq Cat1 Cat1 = Yes Refl
    decEq Cat2 Cat2 = Yes Refl
    decEq Cat1 Cat2 = No cat1NotCat2
    decEq Cat2 Cat1 = No $ negEqSym cat1NotCat2

Show Cat where
    show Cat1 = "Cat1"
    show Cat2 = "Cat2"

readCat : String -> Maybe Cat
readCat "Cat1" = Just Cat1
readCat "Cat2" = Just Cat2
readCat _ = Nothing

data SubCat = SubCat1 | SubCat2 | SubCat3

DecEq SubCat where
    decEq SubCat1 SubCat1 = Yes Refl
    decEq SubCat2 SubCat2 = Yes Refl
    decEq SubCat3 SubCat3 = Yes Refl
    decEq _ _ = No believe_me

Show SubCat where
    show SubCat1 = "SubCat1"
    show SubCat2 = "SubCat2"
    show SubCat3 = "SubCat3"

readSubCat : String -> Maybe SubCat
readSubCat "SubCat1" = Just SubCat1
readSubCat "SubCat2" = Just SubCat2
readSubCat "SubCat3" = Just SubCat3
readSubCat _ = Nothing

validSubCats : Cat -> List SubCat
validSubCats Cat1 = [SubCat1, SubCat2]
validSubCats Cat2 = [SubCat2, SubCat3]

data Item: Cat -> SubCat -> Type where
    MkItem : Elem subCat (validSubCats cat) -> Item cat subCat
data SomeItem : Type where
    MkSomeItem : Item cat subCat -> SomeItem

{cat : Cat} -> {subCat : SubCat} -> Show (Item cat subCat) where
    show (MkItem _) = "Item " ++ show cat ++ " " ++ show subCat

buildItem : (cat : Cat) -> (subCat : SubCat) -> Maybe (Item cat subCat)
buildItem cat subCat = let elem = isElem subCat (validSubCats cat)
                       in case elem of
                           Yes elem => Just $ MkItem elem
                           No _ => Nothing

makeItem : String -> String -> Maybe SomeItem
makeItem cat subCat = do
    cat <- readCat cat
    subCat <- readSubCat subCat
    MkSomeItem <$> buildItem cat subCat
```
