# Calculating a sum in pipes

In [the previous post](/post/96263930992/grouping-in-pipes), I wrote some functions that group values in a pipe. Now, I'd like to calculate a sum in a pipe.

Obviously, we can apply `Prelude.sum` to each list of values like this.

    import Control.Monad (forever)
    import Data.Maybe (catMaybes)
    import Pipes
    import qualified Pipes.Prelude as P

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    test :: Int -> IO ()
    test n = runEffect $ (source >-> P.map Just >> forever (yield Nothing)) >->
                         (forever $ sequence (replicate n await) >>= yield) >->
                         P.map catMaybes >->
                         P.takeWhile (not . null) >->
                         P.map sum >->
                         P.print

Of course, we can calculate a sum in an intermediate pipe instead of calculating it in the last pipe.

    import Control.Monad (forever)
    import Data.Foldable (foldMap)
    import Data.Maybe (fromJust, isJust)
    import Data.Monoid (Monoid, Sum(Sum), getSum)
    import Pipes
    import qualified Pipes.Prelude as P
    import Prelude hiding (sum)

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    --sum :: Num a => [Maybe a] -> Maybe a
    --sum :: (Functor t, Foldable t, Num a) => t (Maybe a) -> Maybe a
    sum :: (Functor t, Foldable t, Functor f, Monoid (f (Sum a))) => t (f a) -> f a
    sum = fmap getSum . foldMap (fmap Sum)

    test :: Int -> IO ()
    test n = runEffect $ (source >-> P.map Just >> forever (yield Nothing)) >->
                         (forever $ sequence (replicate n await) >>= yield . sum) >->
                         P.takeWhile isJust >->
                         P.map fromJust >->
                         P.print

With [pipes-parse](http://hackage.haskell.org/package/pipes-parse), we can fold values into a sum of the values. Note that we split the producer so that it just yields a specified number of values using `splitAt` with `zoom`.

    import Data.Foldable (for_)
    import Lens.Family2.State.Strict (zoom)
    import Pipes
    import Pipes.Parse
    import qualified Pipes.Prelude as P
    import Prelude hiding (splitAt, sum)

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    sum :: (Monad m, Num a) => Int -> Producer a m r -> Producer a m ()
    sum n producer = do
        (m, producer') <- lift $ runStateT parser producer
        for_ m $ \m -> do
            yield m
            sum n producer'
      where
        parser = zoom (splitAt n) $ foldAll add Nothing id
        add (Just x) y = Just $ x + y
        add Nothing y = Just y

    test :: Int -> IO ()
    test n = runEffect $ sum n source >-> P.print

Finally, with [pipes-group](http://hackage.haskell.org/package/pipes-group), we can use `Pipes.Group.folds` to fold values in each chunk.

    import Lens.Family2 (view)
    import Pipes
    import Pipes.Group
    import qualified Pipes.Prelude as P
    import Prelude hiding (sum)

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    sum :: (Monad m, Num a) => Int -> Producer a m r -> Producer a m r
    sum n = folds (+) 0 id . view (chunksOf n)

    test :: Int -> IO ()
    test n = runEffect $ sum n source >-> P.print

With [foldl](http://hackage.haskell.org/package/foldl), you can use `Control.Foldl.sum` instead of writing a folding function by yourself.

    import qualified Control.Foldl as F
    import Lens.Family2 (view)
    import Pipes
    import Pipes.Group
    import qualified Pipes.Prelude as P
    import Prelude hiding (sum)

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    sum :: (Monad m, Num a) => Int -> Producer a m r -> Producer a m r
    sum n = F.purely folds F.sum . view (chunksOf n)

    test :: Int -> IO ()
    test n = runEffect $ sum n source >-> P.print
