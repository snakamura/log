# Why do you need singletons?

Imagine you have a type `Value` indexed by `ValType` kind, and an existential type `SomeValue` that wraps it.

```
{-# LANGUAGE AllowAmbiguousTypes,
             DataKinds,
             GADTs,
             KindSignatures,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeApplications
#-}

import Data.Text (Text)

data ValType = Text | Bool

data Value (tag :: ValType) where
    T :: Text -> Value 'Text
    B :: Bool -> Value 'Bool
deriving instance Show (Value tag)

data SomeValue = forall tag. SomeValue (Value tag)
```

Now, you want to have a function that unwraps a `Value` from a `SomeValue`. This can be done by using singletons.

```
data SValType (tag :: ValType) where
    SText :: SValType 'Text
    SBool :: SValType 'Bool

class SValTypeI (tag :: ValType) where sing :: SValType tag
instance SValTypeI 'Text where sing = SText
instance SValTypeI 'Bool where sing = SBool

unwrap :: forall tag. SValTypeI tag => SomeValue -> Maybe (Value tag)
unwrap (SomeValue v) =
    case sing @tag of
        SBool | B _  Just v
        SText | T _  Just v
        _ -> Nothing
```

But doesn't this look redundant? Why can't you get a `ValueType` directly instead of getting `SValType` like this?

```
class IsType (a :: ValType) where typeOf :: ValType
instance IsType 'Text where typeOf = Text
instance IsType 'Bool where typeOf = Bool

unwarp' :: forall tag. IsType tag => SomeValue -> Maybe (Value tag)
unwarp' (SomeValue v) =
    case typeOf @tag of
        Bool | B _  Just v
        Text | T _  Just v
        _ -> Nothing
```

But no, this doesn't compile. But why?

You can think about this in this way. Passing a typeclass to a function is identical to passing a dictionary of functions to the function. So this `unwrap'` is identical to this definition because `IsType` only has `typeOf` that returns `ValType`.

```
unwarpExplicit' :: ValType -> SomeValue -> Maybe (Value tag)
unwarpExplicit' valType (SomeValue v) =
    case valType of
        Bool | B _  Just v
        Text | T _  Just v
        _ -> Nothing
```

But as you can see, there are no relationships between `ValType` and `tag`. That's why this doesn't compile. `valueType` being `Bool` doesn't mean `tag` is `'Bool`, and you cannot pattern match `v` with `B _`.

On the other hand, it'll become this when you convert the original `wrap` to pass a dictionary explicitly.

```
unwrapExplicit :: SValType tag -> SomeValue -> Maybe (Value tag)
unwrapExplicit sValType (SomeValue v) =
    case sValType of
        SBool | B _  Just v
        SText | T _  Just v
        _ -> Nothing
```

As you can see, `SValType tag` has `tag` which will be unified with `tag` in `Maybe (Value tag)`. When you specify a return type of this function, you also fix `sValType`. For example, `unwrapExplicit SBool (SomeValue (B False)) :: Maybe (Value 'Bool)` compiles, but `unwrapExplicit SText (SomeValue (B False)) :: Maybe (Value 'Bool)` doesn't.

If `sValueType` is `SBool`, the return type must be `Maybe (Value 'Bool)`, and if `SText`, it must be `Maybe (Value 'Text)`. This makes it possible to pattern match on `v` with `B _` and `T _`.
