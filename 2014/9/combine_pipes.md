# Combining pipes

There are number of ways to combine `Producer`s, `Pipe`s and `Consumer`s in [pipes](http://hackage.haskell.org/package/pipes).

Let's take an example and see what we can do. First define a source and a target. This time, we'll use `each [1..9]` as a source and use `Pipes.Prelude.print` as a target.

    > import Data.Functor
    > import Pipes
    > import qualified Pipes.Prelude as P
    >
    > source :: Monad m => Producer Int m ()
    > source = each [1..9]
    >
    > target :: (MonadIO m, Show a) => Consumer a m ()
    > target = P.print

Then, define projection functions we're going to use.

    > double :: Num a => a -> a
    > double = (* 2)
    >
    > plus10 :: Num a => a -> a
    > plus10 = (+ 10)

The simplest way of applying these functions to values is creating `Pipe`s and combining them using `>->`.

    > test :: IO ()
    > test = runEffect $ source >-> P.map double >-> P.map plus10 >-> target

As you see, you can apply a projection function to each value that streams in pipes using `Pipes.Prelude.map`.

Instead of combining `Pipe`s, you can combine `Producer`s. When you create a function of type `a -> Producer b m r`, you can combine them using `for` and `~>`.

First convert a projection function to a function that returns a producer that `yield`s the argument.

    > yieldMap :: Monad m => (a -> b) -> a -> Producer b m ()
    > yieldMap f = yield . f
    >
    > doubleP :: (Monad m, Num a) => a -> Producer a m ()
    > -- doubleP x = yield $ double x
    > doubleP = yieldMap double
    >
    > plus10P :: (Monad m, Num a) => a -> Producer a m ()
    > -- plus10P x = yield $ plus10 x
    > plus10P = yieldMap plus10

Then you can use `for` to combine them.

    > testP1 :: IO ()
    > testP1 = runEffect $ for (for source doubleP) plus10P >-> target

Instead of nesting multiples `for`s, you can combine these functions using `~>`.

    > testP2 :: IO ()
    > testP2 = runEffect $ for source (doubleP ~> plus10P) >-> target

You can even combine them further and make them a producer by passing `()`.

    > testP3 :: IO ()
    > testP3 = runEffect $ (const source ~> doubleP ~> plus10P) () >-> target

On the other hand, you can combine `Consumer`s using `>~`. First create a consumer that applies a projection function to a value fetched using `await`.

    > awaitMap :: Monad m => (a -> b) -> Consumer a m b
    > awaitMap f = f <$> await
    >
    > doubleC :: (Monad m, Num a) => Consumer a m a
    > -- doubleC = do
    > --     x <- await
    > --     return $ double x
    > doubleC = awaitMap double
    >
    > plus10C :: (Monad m, Num a) => Consumer a m a
    > -- plus10C = do
    > --     x <- await
    > --     return $ plus10 x
    > plus10C = awaitMap plus10

Then you can combine them with `>~`.

    > testC :: IO ()
    > testC = runEffect $ source >-> (doubleC >~ plus10C >~ target)

The final approach is using functions that convert a producer to another producer.

    > producerMap :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
    > producerMap f p = do
    >     x <- lift $ next p
    >     case x of
    >         Left r -> return r
    >         Right (v, p') -> do
    >             yield $ f v
    >             producerMap f p'
    >
    > doublePP :: (Monad m, Num a) => Producer a m r -> Producer a m r
    > doublePP = producerMap double
    >
    > plus10PP :: (Monad m, Num a) => Producer a m r -> Producer a m r
    > plus10PP = producerMap plus10
    >
    > testPP :: IO ()
    > testPP = runEffect $ plus10PP (doublePP source) >-> target
