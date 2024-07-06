# Working with record fields in Haskell 2023

There are several ways to work with record fields in Haskell. The most traditional way is to use the ordinal pattern match and record field selectors.

```
{-# LANGUAGE RecordWildCards #-}

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show)

person :: Person
person = Person (Name "Tiger" "Scott") 10

f1, f2, f3, f4 :: String
Person {name = Name {first = f1}} = person
f2 = first $ name person
f3 = (\Person {name = Name {first}} -> first) person
f4 = (\Person {name = Name {..}, ..} -> first) person

a1, a2, a3, a4 :: Int
Person {age = a1} = person
a2 = age person
a3 = (\Person {age} -> age) person
a4 = (\Person {..} -> age) person

p1, p2 :: Person
p1 =
  person
    { name =
        (name person)
          { first = "Micheal"
          }
    }
p2 = person {age = 11}
```

As you can see, getting fields is simple and even simpler with [`NamedFieldPuns`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html) and [`RecordWildCards`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html).

But it suddenly gets rather complex when it comes to updating nested fields.

Note that you can stop generating field selectors with [`NoFieldSelectors`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html). This extension can be useful especially with [`DuplicateRecordFields`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html), which allows you to have multiple fields with the same name in your module.

There is another way to get field values, which is [`HasField`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html). Using `HasField`, you can access a field with a type-level string.

In addition to that, you can enable [`OverloadedLabels`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_labels.html) and add an orphan instance of `IsLabel` to use labels. For instance, you can write `#name` instead of `getField @"name"`.

```
{-# LANGUAGE DataKinds, OverloadedLabels #-}

import GHC.Records
import GHC.OverloadedLabels

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show)

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

person :: Person
person = Person (Name "Tiger" "Scott") 10

f1, f2 :: String
f1 = getField @"first" $ getField @"name" person
f2 = #first $ #name person

a :: Int
a = getField @"age" person
```

Unfortunately, `HasField` doesn't support updating field values at this moment.

[`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html) is another extension which allows you to access a field just like OO languages.

```
{-# LANGUAGE OverloadedRecordDot #-}

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show)

person :: Person
person = Person (Name "Tiger" "Scott") 10

f :: String
f = person.name.first

a :: Int
a = person.age

-- We need to implement setField by ourselves and enable OverloadedRecordUpdate to use these.
{-
p1, p2 :: Person
p1 = person {name.first = "Micheal"}
p2 = person {age = 11}
-}
```

As you can see, you can concatenate field names with `.` to access it. Note that it uses `HasField` under the hood.

There is another extension named [`OverloadedRecordUpdate`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_update.html), but you need to implement `HasField` type class by yourself to make it work.

The next option is [`lens`](https://hackage.haskell.org/package/lens) (or other lens package). Even though the power of `lens` is far stronger than just accessing record fields, it works well just to get and set field values.

```
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Person = Person
  { _name :: Name,
    _age :: Int
  }
  deriving (Show)

data Name = Name
  { _first :: String,
    _last :: String
  }
  deriving (Show)

makeLenses ''Person
makeLenses ''Name

person :: Person
person = Person (Name "Tiger" "Scott") 10

f :: String
f = person ^. name . first

a :: Int
a = person ^. age

p1, p2 :: Person
p1 = person & name . first .~ "Micheal"
p2 = person & age .~ 11
```

While `lens` package uses Template Haskell to generate lenses such as `name`, `age`, `first`, and so on, [`generic-lens`](https://hackage.haskell.org/package/generic-lens) package uses [`GHC.Generics`](https://hackage.haskell.org/package/base/docs/GHC-Generics.html) to generate lenses.

```
{-# LANGUAGE DataKinds, OverloadedLabels #-}

import Control.Lens
import Data.Generics.Labels ()
import Data.Generics.Product
import GHC.Generics

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show, Generic)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show, Generic)

person :: Person
person = Person (Name "Tiger" "Scott") 10

f1, f2 :: String
f1 = person ^. field @"name" . field @"first"
f2 = person ^. #name . #first

a1, a2 :: Int
a1 = person ^. field @"age"
a2 = person ^. #age

p1, p2, p3 :: Person
p1 = person & field @"name" . field @"first" .~ "Micheal"
p2 = person & field @"age" .~ 11
p3 = person & #name . #first .~ "Micheal"
```

As you can see it uses a type-level string with `field` to specify a field instead of a generated lens. You can also import instances from `Data.Generics.Labels` to use labels instead, for instance, `#name` instead of `field @"name"`.

There are some libraries that support record-like type if you take options to use non-standard records.

For example, [extensible](https://hackage.haskell.org/package/extensible) lets you to define a record type with `Record` and work with it using lenses.

```
{-# LANGUAGE DataKinds, OverloadedLabels #-}

import Control.Lens
import Data.Extensible

type Person = Record '[ "name" >: Name
                      , "age" >: Int
                      ]

type Name = Record '[ "first" >: String
                    , "last" >: String
                    ]

person :: Person
person = #name @= (    #first @= "Tiger"
                    <: #last @= "Scott"
                    <: nil
                  )
      <: #age @= 10
      <: nil

f :: String
f = person ^. xlb #name . xlb #first

a :: Int
a = person ^. #age

p1, p2 :: Person
p1 = person & xlb #name . xlb #first .~ "Micheal"
p2 = person & #age .~ 11
```

One thing you should know is that you need `xlb` when you combine lenses.

[vinyl](https://hackage.haskell.org/package/vinyl) provides similar functionalities.

```
{-# LANGUAGE DataKinds, OverloadedLabels #-}

import Control.Lens
import Data.Vinyl
import Data.Vinyl.Syntax ()

type Person = FieldRec '[ "name" ::: Name
                        , "age" ::: Int
                        ]

type Name = FieldRec '[ "first" ::: String
                      , "last" ::: String
                      ]

person :: Person
person = #name =:= (     #first =:= "Tiger"
                     <+> #last =:= "Scott"
                   )
     <+> #age =:= 10

f :: String
f = person ^. #name . #first

a :: Int
a = person ^. #age

p1, p2 :: Person
p1 = person & #name . #first .~ "Micheal"
p2 = person & #age .~ 11
```

Note that these libraries provides much more functionalities than just replacing standard records. I picked up very basic usages in this post though.
