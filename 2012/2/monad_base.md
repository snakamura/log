# `MonadBase` is a generalized `MonadIO`

When you work with monad transformers, you may lift a function using [lift](http://hackage.haskell.org/packages/archive/transformers/0.2.2.0/doc/html/Control-Monad-Trans-Class.html#v:lift). For example, this code lifts `print` from the base IO monad.

    test = runMaybeT go
      where
        go = do lift $ print "test"
                return 3

But with `lift` you need to lift multiple times. For example, this code doesn't compile.

    test5 = runMaybeT (runStateT go 0)
        where
          go = do lift $ print "test"
                  return 3

You need to lift two times explicitly.

    test5 = runMaybeT (runStateT go 0)
        where
          go = do lift $ lift $ print "test"
                  return 3

With MonadIO, you can lift a function in the base IO monad through arbitrary levels. Now you can write:

    test5 = runMaybeT (runStateT go 0)
        where
          go = do liftIO $ print "test"
                  return 3

[MonadBase](http://hackage.haskell.org/packages/archive/transformers-base/0.4.1/doc/html/Control-Monad-Base.html) is a generalized version of MonadIO that allows you to lift a function in a base monad through arbitrary levels. The base monads include List, Maybe, ST, STM, IO and so on.

For example, you can access STRef in the base monad directly using liftBase.

    test = runST go
        where
          go = do r
