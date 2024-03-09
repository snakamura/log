# Convert a type level natural number to a value level integer and vise versa

As we saw in [the last post](http://snak.tumblr.com/post/109858446827/type-safe-list-with-ghc-typelits), GHC supports type level natural numbers. How can we treat it in value level?

To bring a type level natural number to a value level, we use [`Data.Proxy.Proxy`](http://hackage.haskell.org/package/base/docs/Data-Proxy.html#t:Proxy). Then use [`GHC.TypeLits.natVal`](http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html#v:natVal) to convert it to `Integer`. For example, `natVal (Proxy :: Proxy 10)` returns `10`.

Let's take an example of the type-safe list in the last post and add `length` function that returns a length of a list.

    length :: forall len a. KnownNat len => List len a -> Integer
    length _ = natVal (Proxy :: Proxy len)

Note that it doesn't use an actual list value. It just uses its type.

On the other hand, we can use [`GHC.TypeLits.someNatVal`](http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html#v:someNatVal) to convert `Integer` to a type level number. At this time, I'm going to add `isLength` function for a type-safe list. Of course, you can implement it using `length`, but I'm not going to do that.

    isLength :: forall len a. KnownNat len => Integer -> List len a -> Bool
    isLength n _ | Just (SomeNat p)  List l a -> List (l + 1) a

    deriving instance Show a => Show (List len a)

    length :: forall len a. KnownNat len => List len a -> Integer
    length _ = natVal (Proxy :: Proxy len)

    isLength :: forall len a. KnownNat len => Integer -> List len a -> Bool
    isLength n _ | Just (SomeNat p)
