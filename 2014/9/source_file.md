# Getting a path to a source file

It's sometimes convenient to get an absolute path to a source file for logging. Template Haskell allows you to do it.

    {-# LANGUAGE TemplateHaskell #-}

    import Control.Monad
    import Data.Functor
    import Language.Haskell.TH
    import System.Directory
    import System.FilePath

    filePath :: String
    filePath = $(liftM2 (</>) (runIO getCurrentDirectory) (loc_filename <$> location) >>= litE . stringL)

If you're thinking about using this for logging, you may be interested in [monad-logger](http://hackage.haskell.org/package/monad-logger).
