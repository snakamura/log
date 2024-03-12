# Lifting a monadic computation through a stack of monad transformers, part 2

In [the previous post](/post/18836381890/lifting-a-monadic-computation-through-a-stack-of-monad), we defined `liftFunc'''` which lifts a computation in IO to MaybeT. If you do the same thing for StateT, you'll get this function.

    liftFunc''' :: ((forall a. StateT s IO a -> IO (a, s)) -> IO (b, s)) -> StateT s IO b
    liftFunc''' f = let z s = f (flip runStateT s)
                    in StateT $ \s -> z s

By comparing `liftFunc'''` for MaybeT and StateT, we'll find that we can generalize `liftFunc'''` by introducing a type class and a type synonym family.

    {-# LANGUAGE TypeFamilies, RankNTypes #-}

    import Control.Monad (liftM)
    import Control.Monad.Trans (MonadTrans, lift)
    import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
    import Control.Monad.Trans.State (StateT(StateT), runStateT, put)
    import Data.Tuple (swap)

    class MonadTrans t => MonadTransFunc t where
        type C t :: * -> *
        liftFunc :: Monad m => ((forall a. t m a -> m (C t a)) -> m (C t b)) -> t m b

The type family C denotes a container that carries a monadic value in the base monad. It's `Maybe` for `MaybeT` and `(a, s)` for `StateT s`. For example, we can define an instance of this class for MaybeT.

    instance MonadTransFunc MaybeT where
        type C MaybeT = Maybe
        liftFunc f = MaybeT $ f runMaybeT

Although we want to define an instance for StateT like:

    instance MonadTransFunc (StateT s) where
        type C (StateT s) a = (a, s)
        liftFunc f = StateT $ \s -> f $ flip runStateT s

we cannot do this because it applies two type parameters to C. So we have to swap the types in the tuple.

    instance MonadTransFunc (StateT s) where
        type C (StateT s) = (,) s
        liftFunc f = StateT $ \s -> liftM swap $ f (liftM swap . flip runStateT s)

Now you can lift computations using `liftFunc`.

    testFunc0 :: IO a -> IO a
    testFunc0 f = print "testFunc0" >> f

    testFunc1 :: (Int -> IO a) -> IO a
    testFunc1 f = print "testFunc1" >> f 1

    maybeFunc0 :: MaybeT IO Int
    maybeFunc0 = do lift $ print "maybeFunc0"
                    return 0

    stateFunc1 :: Int -> StateT Int IO Int
    stateFunc1 n = do lift $ print $ "stateFunc1: " ++ show n
                      put n
                      return n

    maybe0 = runMaybeT go
        where
          go = liftFunc $ \run -> testFunc0 $ run $ maybeFunc0

    state1 = runStateT go 0
        where
          go = liftFunc $ \run -> testFunc1 $ \x -> run $ stateFunc1 x

Note that you can lift a computation through two levels by applying `liftFunc` twice.

    maybeStateFunc1 :: Int -> MaybeT (StateT Int IO) Int
    maybeStateFunc1 n = do lift $ lift $ print $ "maybeStateFunc1: " ++ show n
                           lift $ put n
                           return n

    maybeState1 = runStateT (runMaybeT go) 0
        where
          go = liftFunc $ \runMaybe ->
                   liftFunc $ \runState ->
                       testFunc1 $ \x -> runState $ runMaybe $ maybeStateFunc1 x

But in most case, you may want to lift computations in a base monad directly through the stack of monad transformers as `liftIO` and `liftBase` do. How can we do that?
