# Catching an exception using `MonadCatchIO`

Imagine you're working with monad transformers and want to catch an exception. But because the signature of [catch](http://hackage.haskell.org/packages/archive/base/4.5.0.0/doc/html/Control-Exception.html#v:catch) is `Exception e => IO a -> (e -> IO a) -> IO a`, you need to unwrap monad stacks to do it.

For example, you can catch an exception inside MaybeT monad like:

    {-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, ScopedTypeVariables #-}

    import Control.Exception
    import Control.Monad.IO.Class
    import Control.Monad.Trans.Maybe
    import Data.Typeable
    import Prelude hiding (catch)

    data TestException = TestException deriving (Show, Typeable)

    instance Exception TestException

    test = runMaybeT $ catchMaybeT (liftIO $ throwIO TestException)
                                   (\(e :: TestException) -> return 0)

    catchMaybeT m f = MaybeT $ runMaybeT m `catch` (\e -> runMaybeT $ f e)

MonadCatchIO class offers this kind of handlers for most monad transformers. You can use [Control.Monad.CatchIO.catch](http://hackage.haskell.org/packages/archive/MonadCatchIO-transformers/latest/doc/html/Control-Monad-CatchIO.html#v:catch) instead of `catchMaybeT` above. It also provides [bracket](http://hackage.haskell.org/packages/archive/MonadCatchIO-transformers/latest/doc/html/Control-Monad-CatchIO.html#v:bracket) families.

But care must be taken when you use bracket with some monad transformers, because the final computation may not run.

For example, in this code, "End" will never be printed.

    test = runMaybeT $ CatchIO.bracket (liftIO $ print "Start")
                                       (const $ liftIO $ print "End")
                                       (const empty)

This is because the final computation runs just after the main computation if no exception is thrown, and once a computation becomes Nothing, the computations following that computation will become Nothing in MaybeT monad. This is just the same as

    runMaybeT $ empty >> liftIO (print "x")

never printing "x".

The same goes for ErrorT monad.

Another point I'd like to mention is about a state when each computation runs in StateT.

    {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

    import Control.Exception
    import Control.Monad.CatchIO as CatchIO
    import Control.Monad.IO.Class
    import Control.Monad.Trans.State
    import Data.Typeable

    data TestException = TestException deriving (Show, Typeable)

    instance Exception TestException

    test f = runStateT go "Init"
        where
          go = do CatchIO.bracket (updateState "Start")
                                  (const $ updateState "End")
                                  (const f)
                      `CatchIO.catch` \(_ :: TestException) -> updateState "Catch"
                  updateState "Last"

    updateState at = do printState at
                        put at
                        return at

    printState at = get >>= \state -> liftIO $ print $ "At " ++ at ++ ": " ++ state

    test1 :: IO (String, String)
    test1 = test $ do updateState "Func"
                      liftIO $ throwIO TestException

In this code, you'll find that the final computation runs with the state at the start, and the catch computation runs with the initial state, if an exception is thrown. I mean any changes made to the state in the main computation are discarded, although any changes in the base IO monad won't be reverted --- actually it's impossible.
