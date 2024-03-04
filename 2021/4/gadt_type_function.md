# Using GADT as a type function

Imagine that you'd like to write a function that works on different types, such as on `Int` and `Text`. You can write this function using a type class.

Then, what can you do when you want to return a different type from this function depending on its argument type? You can write it using a type family.

```
{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             GADTs,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             TypeApplications,
             TypeFamilies
#-}
{-# OPTIONS -Wall #-}

import Data.Text
import qualified Data.Text as T
import Text.Read (readMaybe)

type family R a where
    R Int = Text
    R Text = Maybe Int

class F a where
    f1 :: a -> R a
instance F Int where
    f1 = T.pack . show
instance F Text where
    f1 = readMaybe . T.unpack
```

I used a closed type family here to make it consistent with the GADT version, but usually you'll use an open type family (you'd call it an associated type).

But you can write this function using GADT, too. First, let's define a GADT that associates an argument type and a return type.

```
data W a r where
    WInt :: W Int Text
    WText :: W Text (Maybe Int)
```

Next, let's write the function. It will take a value of this type in addition to the original argument.

```
f2 :: W a r -> a -> r
f2 WInt = T.pack . show
f2 WText = readMaybe . T.unpack
```

When you pass `WInt` to `f2`, the compiler unifies `a` with `Int` and then `r` with `Text`. When you pass `WText`, `a` will be unified with `Text` and `r` with `Maybe Int`.

Since `WInt` and `WText` is in the value world, you can say that you're converting a value (`WInt`) to types (`Int` and `Text`).

Instead of passing `W a r` explicitly, you can let the compiler infer it using a type class.

```
class WC a r | a -> r where
    w :: W a r
instance WC Int Text where
    w = WInt
instance WC Text (Maybe Int) where
    w = WText

f3 :: forall a r. WC a r => a -> r
f3 x = case w @a of
         WInt -> T.pack $ show x
         WText -> readMaybe $ T.unpack x
```

When you pass a value of `a` to `f3`, the compiler infers `W a r` using `w`. Then, it infers `r` using the functional dependency `a -> r`.
