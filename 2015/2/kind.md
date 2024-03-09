# Type-safe list, DataKinds, ConstraintKinds and more

I've recently learned GHC's extensions for kinds such as `DataKinds` and `ConstraintKinds`, and I think it's worth leaving some notes about them. To make it clear which extension does what, I'm going to use them step-by-step, although you may feel it redundant. Let's take a type-safe list as an example.

# 1. Type-safe list

Here is a basic implementation of type-safe list.

    {-# LANGUAGE GADTs,
                 NoImplicitPrelude #-}

    data Z
    data S n

    data List len a where
        Nil :: List Z a
        Cons :: a -> List l a -> List (S l) a

    head :: List (S l) a -> a
    head (Cons a _) = a

    tail :: List (S l) a -> List l a
    tail (Cons _ r) = r

This `List` type has a phantom type of `len` which keeps track of a length of a list. So, for example, applying `head` and `tail` to `Nil` makes the type-checker fail.

    *Main> head Nil

    <interactive>:3:6:
        Couldn't match type ‘Z’ with ‘S l0’
        Expected type: List (S l0) a
          Actual type: List Z a
        In the first argument of ‘head’, namely ‘Nil’
        In the expression: head Nil

# 2. Make it an instance of Show

Before starting playing with it, I'd like to make `List` an instance of `Show` so that we can print it.

    {-# LANGUAGE GADTs,
                 NoImplicitPrelude,
                 StandaloneDeriving #-}

    import Prelude (Show)

    data Z
    data S n

    data List len a where
        Nil :: List Z a
        Cons :: a -> List l a -> List (S l) a

    deriving instance Show a => Show (List len a)

I just enabled `StandaloneDeriving` extension and added a standalone deriving declaration to make it an instance of `Show`. Note that a compiler complains if you add `deriving Show` directly to `List`.

# 3. Introduce Num kind

One possible flaw in this implementation is that it's possible to specify an arbitrary type as `len`. For example, the following snippet type-checks. This is because the kind of `len` is `*`, so you can apply any type whose kind is `*` to it.

    *Main Prelude> :t undefined :: List Int Char
    undefined :: List Int Char :: List Int Char

To check its kind, you can use `:kind` in GHCi.

    *Main Prelude> :kind List
    List :: * -> * -> *

To avoid this flaw, you can introduce your own kind using `DataKinds` extension.

    {-# LANGUAGE DataKinds,
                 GADTs,
                 NoImplicitPrelude,
                 StandaloneDeriving #-}

    import Prelude (Show)

    data Num = Z | S Num

    data List len a where
        Nil :: List Z a
        Cons :: a -> List l a -> List (S l) a

    deriving instance Show a => Show (List len a)

When you enable `DataKinds`, GHC creates a kind for a corresponding type, and types for correspoinding data constructors.

    data Num = Z | S Num

The above snippet defines `Num` kind, and `Z` and `S` type constructor, as well an ordinal `Num` type. It's something like this pseudo code.

    data Num = Z | S Sum -- This defines an ordinal type
    kind Num -- This defines Num kind
    data Z :: Num -- This defines Z type whose kind is Num
    data S Num :: Num -- This define S type whose kind is Num -> Num

This can become tricky because `Bool` can be interpreted as a type or a kind depending on where it is used. The same goes for `True` and `False`, which can be interpreted as types or values (data constructors). It's vital to distinguish these two interpretations when you read and write code that works on types.

With this definition, the compiler doesn't type-check the last example.

    *Main Prelude> :t undefined :: List Int Char

    <interactive>:1:19:
        The first argument of ‘List’ should have kind ‘Main.Num’,
          but ‘Int’ has kind ‘*’
        In an expression type signature: List Int Char
        In the expression: undefined :: List Int Char

Also, you can check its kind.

    *Main Prelude> :kind List
    List :: Main.Num -> * -> *

# 4. Add a kind signature

In the example in the previous section, it's not obvious that a kind of `len` is `Num`. When you enable `KindSignatures` extension, you can explicitly write its kind.

    {-# LANGUAGE DataKinds,
                 GADTs,
                 KindSignatures,
                 NoImplicitPrelude,
                 StandaloneDeriving #-}

    import Prelude (Show)

    data Num = Z | S Num

    data List (len :: Num) a where
        Nil :: List Z a
        Cons :: a -> List l a -> List (S l) a

    deriving instance Show a => Show (List len a)

# 5. Use constraints

Let's get back to the first example. The type signature of `head` was this.

    head :: List (S l) a -> a

This type signature doesn't directly state its restriction of "if the list is not empty." How we can write this restriction directly? If it's "if the list is empty," we can write it like this by making it a constraint.

    head :: l ~ Z => List l a -> a

Unfortunately, there is no `/~` operator which means that the type is different. But we introduce it by ourselves using `TypeFamilies`.

    {-# LANGUAGE DataKinds,
                 GADTs,
                 KindSignatures,
                 NoImplicitPrelude,
                 StandaloneDeriving,
                 TypeFamilies #-}

    import Prelude (Bool(True, False), Show)

    data Num = Z | S Num

    data List len a where
        Nil :: List Z a
        Cons :: a -> List l a -> List (S l) a

    deriving instance Show a => Show (List len a)

    type family NotZ (len :: Num) :: Bool where
        NotZ Z = False
        NotZ (S n) = True

    head :: NotZ l ~ True => List l a -> a
    head (Cons a _) = a

Since we enabled `DataKinds` extension, there is `Bool` kind, `True` and `False` types, as we saw in the previous sections. `NotZ` type family becomes `True` when it's applied to `S`, and `False` it's applied to `Z`.

This new type signature for `head` states that it can be applied to a list whose length satisfies `NotZ l ~ True`.

# 6. Use ConstraintKinds

It's not clear enough what `NotZ l ~ True` means. When you enable `ConstraintKinds` extension, you can declare a type whose kind is `Constraint`, which can be placed at the left side of `=>` in type signatures.

The above example can be written as follows when you enable `ConstraintKinds`.

    type NotZero a = NotZ a ~ True

    head :: NotZero l => List l a -> a
    head (Cons a _) = a

It became much clearer that `head` can be applied to a list whose length isn't zero. If you would really like to make a constraint look directly applied to a list type, you could also write something like the following.

    {-# LANGUAGE ConstraintKinds,
                 DataKinds,
                 GADTs,
                 KindSignatures,
                 NoImplicitPrelude,
                 StandaloneDeriving,
                 TypeFamilies #-}

    import Prelude (Bool(True, False), Show)

    data Num = Z | S Num

    data List len a where
        Nil :: List Z a
        Cons :: a -> List l a -> List (S l) a

    deriving instance Show a => Show (List len a)

    type family NotEmpty' (list :: Num -> * -> *) (l :: Num) :: Bool where
        NotEmpty' list Z = False
        NotEmpty' list (S n) = True

    type NotEmpty list len = NotEmpty' list len ~ True

    head :: NotEmpty List l => List l a -> a
    head (Cons a _) = a
