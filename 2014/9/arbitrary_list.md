# Writing an instance of `Arbitrary` for a list

Imagine you have two functions which convert Word32 to its bit representation and vise versa. These functions would be something like this.

    toBits :: Word32 -> [Bool]
    toBits = ...

    fromBits :: [Bool] -> Word32
    fromBits = ...

Their implementations are trivial, but I'd like to write tests for them. With [doctest](http://hackage.haskell.org/package/doctest), you can write [QuickCheck](http://hackage.haskell.org/package/QuickCheck) properties for them as comments.

I'm going to write two properties; one of them is that `fromBit (toBit n) == n` and the other is `toBit (fromBit bits) == bits`. You can write the former easily.

    -- |
    -- prop> \n -> fromBits (toBits n) == (n :: Word32)
    toBits :: Word32 -> [Bool]
    toBits = ...

But the latter is not that simple because we have to restrict QuickCheck to generate lists of `Bool` which is no longer than 32 in its length. To do that, I cannot use the default instance of Arbitrary for a list. Instead, I need to define its newtype wrapper by ourselves.

With doctest, I can write these declarations in `$setup` named chunk.

    -- $setup
    -- >>> import Test.QuickCheck
    -- >>>
    -- >>> newtype Bits = Bits { getBits :: [Bool] } deriving Show
    -- >>> instance Arbitrary Bits where arbitrary = Bits <$> resize (finiteBitSize (undefined :: Word32)) arbitrary

I'm using `resize` to tell a generator to generate lists which is no longer than 32. With this instance, I can write the property like this.

    -- |
    -- prop> \bits -> toBits (fromBits (getBits bits) :: Word32) == getBits bits
    fromBits :: [Bool] -> Word32
    fromBits = ...
