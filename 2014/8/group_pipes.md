# Grouping in pipes

While playing with [pipes](http://hackage.haskell.org/package/pipes), I was wondering how I could group values in a pipe. As an exercise, I tried to create a producer that yields lists of values from a producer that yields values. This may not make sense because streaming libraries like pipes are used not to load all values in a list, but it's just an exercise.

The first attempt was creating a pipe that calls `Pipes.await` multiple times and yields values as a list.

    import Control.Monad (forever)
    import Pipes
    import qualified Pipes.Prelude as P

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    gather :: Monad m => Int -> Pipe a [a] m r
    gather n = forever $ sequence (replicate n await) >>= yield

    test :: Int -> IO ()
    test n = runEffect $ source >-> gather n >-> P.print

The problem of this attempt was that it didn't yield last values if there weren't enough numbers of values. For example, `test 2` doesn't yield `9` at the end of the stream.

    >>> test 2
    [1,2]
    [3,4]
    [5,6]
    [7,8]

I'd rather like it to yield `[9]` at the end. One solution that I came up with was putting all values into `Just` and appending infinite `Nothing` at the end of the stream, then taking only `Just`s from the stream.

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
                         P.print

This worked pretty well.

Next, since it seemed impossible to implement this as a pipe, I tried to create a function from `Producer a m r` to `Producer [a] m r`.

    import Control.Monad (unless)
    import Pipes
    import qualified Pipes.Prelude as P

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    gather :: Monad m => Int -> Producer a m r -> Producer [a] m r
    gather n producer = go n producer []
      where
        go 0 producer l = do
            yield $ reverse l
            go n producer []
        go m producer l = do
            x <- lift $ next producer
            case x of
                Left r -> do
                    unless (null l) $
                        yield $ reverse l
                    return r
                Right (v, producer') -> go (m - 1) producer' (v:l)

    test :: Int -> IO ()
    test n = runEffect $ gather n source >-> P.print

`gather` retrieves each value from the producer and yields a list of values when it retrieves a specified number of values, or it reaches at the end of the producer.

Then I thought it was easier if I used [pipes-parse](http://hackage.haskell.org/package/pipes-parse).

    import Control.Monad (unless)
    import Data.Functor ((<$>))
    import Data.Maybe (catMaybes)
    import Pipes
    import Pipes.Parse
    import qualified Pipes.Prelude as P

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    gather :: (Functor m, Monad m) => Int -> Producer a m r -> Producer [a] m ()
    gather n producer = do
        (l, producer') <- lift $ runStateT parser producer
        unless (null l) $ do
            yield l
            gather n producer'
      where
        parser = catMaybes <$> sequence (replicate n draw)

    test :: Int -> IO ()
    test n = runEffect $ gather n source >-> P.print

This is basically the same way as the previous attempt, but uses pipes-parse to retrieve values from the producer. With pipes-parse, we don't need to retrieve values one by one, but we no longer be able to return `r` and always return `()`.

With pipes-parse, you can also `zoom` to a producer split by `Pipes.Parse.splitAt`. If you split the producer, you can get all values using `Pipes.Parse.drawAll` instead of calling `Pipes.Parse.draw` specified times, because now that the split producer only yields the specified number of values.

    import Control.Monad (unless)
    import Lens.Family2.State.Strict (zoom)
    import Pipes
    import Pipes.Parse
    import qualified Pipes.Prelude as P
    import Prelude hiding (splitAt)

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    gather :: Monad m => Int -> Producer a m r -> Producer [a] m ()
    gather n producer = do
        (l, producer') <- lift $ runStateT parser producer
        unless (null l) $ do
            yield l
            gather n producer'
      where
        parser = zoom (splitAt n) drawAll

    test :: Int -> IO ()
    test n = runEffect $ gather n source >-> P.print

Another approach was to use [pipes-group](http://hackage.haskell.org/package/pipes-group). Once you write a function that retrieves all values from a producer and yields them as a list, you can apply to a producer split by `Pipes.Group.chunksOf` using `Pipes.Group.maps`.

    import Lens.Family2 (view)
    import Pipes
    import Pipes.Group
    import qualified Pipes.Prelude as P

    source :: Monad m => Producer Int m ()
    source = each [1..9]

    gather :: Monad m => Int -> Producer a m r -> Producer [a] m r
    gather n = concats . maps toListProducer . view (chunksOf n)

    toListProducer :: Monad m => Producer a m r -> Producer [a] m r
    toListProducer producer = go producer []
      where
        go producer l = do
            x <- lift $ next producer
            case x of
                Left r -> do
                    yield $ reverse l
                    return r
                Right (v, producer') -> go producer' (v:l)

    test :: Int -> IO ()
    test n = runEffect $ gather n source >-> P.print

The problem is that there seems no way to write `toListProducer` in a smart manner. Note that we cannot use the following definition, because it always returns `()`. `maps` needs a function from a producer to a producer which can return an arbitrary `r`.

    toListProducer' :: Monad m => Producer a m () -> Producer [a] m ()
    toListProducer' p = lift (P.toListM p) >>= yield
