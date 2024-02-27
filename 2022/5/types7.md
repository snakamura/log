# Expressing relations between types, part 7

By putting everything together, we'll get this. [The original code](https://snak.tumblr.com/post/684284139560681472/expressing-relations-between-types-part-1) was 34 lines and this one is 154 lines. You've got type-safety, but they have the same level of safety when you look at them from outside. Do you think it's worth it?

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
    , cat
    , subCat
    , type Cat(..)
    , SCat(..)
    , type SubCat(..)
    , SSubCat(..)
    , buildItem
    , makeItem
    ) where

import Data.Functor (($>))
import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (:~:)(Refl), TestEquality, testEquality)
import Text.ParserCombinators.ReadP (choice, readP_to_S, string)
import Text.Read (readMaybe)

data Cat = Cat1 | Cat2

type SCat :: Cat -> Type
data SCat t where
    SCat1 :: SCat 'Cat1
    SCat2 :: SCat 'Cat2

instance Show (SCat cat) where
    show SCat1 = "Cat1"
    show SCat2 = "Cat2"

type SCatI :: Cat -> Constraint
class SCatI cat where
    singCat :: SCat cat
instance SCatI 'Cat1 where
    singCat = SCat1
instance SCatI 'Cat2 where
    singCat = SCat2

type SomeSCat :: Type
data SomeSCat where
    SomeSCat :: SCatI cat => SCat cat -> SomeSCat

instance Show SomeSCat where
    show (SomeSCat sCat) = show sCat

instance Read SomeSCat where
    readsPrec _ = readP_to_S $ choice [ string "Cat1" $> SomeSCat SCat1
                                      , string "Cat2" $> SomeSCat SCat2
                                      ]

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

instance TestEquality SSubCat where
    testEquality SSubCat1 SSubCat1 = Just Refl
    testEquality SSubCat2 SSubCat2 = Just Refl
    testEquality SSubCat3 SSubCat3 = Just Refl
    testEquality _ _ = Nothing

type ValidSubCats :: Cat -> [SubCat]
type family ValidSubCats cat where
    ValidSubCats 'Cat1 = '[ 'SubCat1, 'SubCat2 ]
    ValidSubCats 'Cat2 = '[ 'SubCat2, 'SubCat3 ]

validSSubCats :: SCat cat -> HL SSubCat (ValidSubCats cat)
validSSubCats SCat1 = SSubCat1 `HCons` SSubCat2 `HCons` HNil
validSSubCats SCat2 = SSubCat2 `HCons` SSubCat3 `HCons` HNil

type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. Elem subCat (ValidSubCats cat) -> Item cat subCat
instance (SCatI cat, SSubCatI subCat) => Show (Item cat subCat) where
    show _ = "Item " <> show (singCat @cat) <> " " <> show (singSubCat @subCat)

data SomeItem where
    SomeItem :: (SCatI cat, SSubCatI subCat) => Item cat subCat -> SomeItem
instance Show SomeItem where
    show (SomeItem item) = show item

cat :: SomeItem -> SomeSCat
cat (SomeItem (Item _ :: Item cat subCat)) = SomeSCat (singCat @cat)

subCat :: SomeItem -> SomeSSubCat
subCat (SomeItem (Item _ :: Item cat subCat)) = SomeSSubCat (singSubCat @subCat)

buildItem :: SCat cat -> SSubCat subCat -> Maybe (Item cat subCat)
buildItem cat subCat = Item <$> hElem subCat (validSSubCats cat)

makeItem :: String -> String -> Maybe SomeItem
makeItem cat subCat = do
    SomeSCat cat <- readMaybe cat
    SomeSSubCat subCat <- readMaybe subCat
    SomeItem <$> buildItem cat subCat


type HL :: (k -> Type) -> [k] -> Type
data HL f xs where
    HCons :: f x -> HL f xs -> HL f (x ': xs)
    HNil :: HL f '[]
infixr 5 `HCons`

type Elem :: k -> [k] -> Type
data Elem t ts where
    Here :: Elem t (t ': ts)
    There :: Elem t ts -> Elem t (t' ': ts)

hElem :: TestEquality f => f x -> HL f xs -> Maybe (Elem x xs)
hElem x (HCons y ys) = case testEquality x y of
                           Just Refl -> Just Here
                           Nothing -> There <$> hElem x ys
hElem _ _ = Nothing
```
