# Hash with Pipes

As [cryptohash](http://hackage.haskell.org/package/cryptohash) is designed to work well with `foldl'`, it can work well with `Pipes.Prelude.fold`. For example, you can write a function to calculate a SHA1 digest of a specified file like this.

    {-# LANGUAGE OverloadedStrings #-}

    import qualified Control.Foldl as F
    import Control.Monad
    import qualified Crypto.Hash as H
    import qualified Data.ByteString as B
    import qualified Data.Vector as V
    import qualified Pipes as P
    import qualified Pipes.ByteString as PB
    import qualified Pipes.Prelude as P
    import System.Environment
    import System.IO

    main :: IO ()
    main = liftM head getArgs >>= hashFile >>= print

    hashFile :: FilePath -> IO (H.Digest H.SHA1)
    hashFile path = withFile path ReadMode $ P.fold H.hashUpdate H.hashInit H.hashFinalize . PB.fromHandle

It's a good idea to factor out a fold which can be used with any `Foldable`s as well.

    hash :: H.HashAlgorithm a => F.Fold B.ByteString (H.Digest a)
    hash = F.Fold H.hashUpdate H.hashInit H.hashFinalize

For example, you can use this with a `Vector`.

    hashVector :: V.Vector B.ByteString -> H.Digest H.SHA1
    hashVector = F.fold hash

    h = hashVector $ V.fromList ["a", "b"]

With this fold, it's easy to write a function that reads a stream of `ByteString`s from a producer and calculates a digest of it.

    hashProducer :: (Monad m, H.HashAlgorithm a) => P.Producer B.ByteString m () -> m (H.Digest a)
    hashProducer = F.purely P.fold hash

Here is a rewritten version of `hashFile` using `hashProducer`.

    hashFile' :: FilePath -> IO (H.Digest H.SHA1)
    hashFile' path = withFile path ReadMode $ hashProducer . PB.fromHandle
