# Lifting a monadic computation through a stack of monad transformers, part 1

We've already know that we can lift a monadic computation using `lift` of `MonadTrans`. For example, we can lift `print` to MaybeT monad like:

    test = runMaybeT go
        where
          go = lift $ print "Test"

But how can we lift a computation that takes monadic computations as its arguments? For example, how can we define this `liftFunc1`?

    testFunc1 :: (Int -> IO a) -> IO a
    testFunc1 f = f 1

    func1 :: Int -> MaybeT IO Int
    func1 n = lift (print n) >> return n

    test1 = runMaybeT go
        where
          go = liftFunc1 testFunc1 func1

The type signature of this function may be like this:

    liftFunc1 :: ((a -> IO b) -> IO b) -> (a -> MaybeT IO b) -> MaybeT IO b

and its definition would be something like this one.

    liftFunc1 f g = let h x = liftM fromJust $ runMaybeT $ g x
                        z = f h
                    in MaybeT $ liftM Just z

But because `f` needs to return `b`, we have to get a value from `Maybe` and re-wrap it in `Just`. As you've notice, it doesn't work if `g` returns `Nothing`.

So we allow the inner computation to wrap its return value with Maybe. The new type signature would be:

    liftFunc1 :: ((a -> IO (Maybe b) -> IO (Maybe b)) -> (a -> MaybeT IO b) -> MaybeT IO b

and the definition would be:

    liftFunc1 f g = let h x = runMaybeT $ g x
                        z = f h
                    in MaybeT z

The basic idea is to run the outer monad (`MaybeT`, this case) and wrap the monadic value with a container (`Maybe`) and make the inner monad return it (`IO (Maybe b)`).

Note that because we made the inner computation to wrap its return value, the function must be polymorphic. You can't lift a computation like this one.

    testFunc :: (Int -> IO Int) -> IO Int

We have defined liftFunc1 that lifts a monadic computation that takes a function that takes one argument. How about a function that takes no argument or two arguments? Those will be:

    liftFunc0 f g = let h = runMaybeT g
                        z = f h
                    in MaybeT z

    liftFunc2 f g = let h x y = runMaybeT $ g x y
                        z = f h
                    in MaybeT z

Now I'd like to define a function that can be used for all of these computations. The very simple approach is moving `h` to callers. If we define `liftFunc'` as

    liftFunc' :: (a -> IO (Maybe b)) -> a -> MaybeT IO b
    liftFunc' f g = let z = f g
                    in MaybeT z

we can write a caller like:

    test' = runMaybeT go
        where
          go = liftFunc' testFunc2 (\x y -> runMaybeT $ func2 x y)

This is fine, but we don't make a caller to call `runMaybeT` directly, because we'd like to make a caller to be used with other monad transformers in the future. So we made `liftFunc''` to pass `runMaybeT` to `g`.

    liftFunc'' :: (a -> IO (Maybe b)) -> ((MaybeT IO c -> IO (Maybe c)) -> a) -> MaybeT IO b
    liftFunc'' f g = let z = f (g runMaybeT)
                         in MaybeT z

    test'' = runMaybeT go
        where
          go = liftFunc'' testFunc2 (\run x y -> run $ func2 x y)

As you've noticed, because `g` will be just given back to `f`, we don't need to pass `g` to `liftFunc''`. Instead of passing `runMaybeT` to `g`, we pass it to `f` and make `liftFunc'''` not take `g` as an argument.

    liftFunc''' :: ((MaybeT IO a -> IO (Maybe a)) -> IO (Maybe b)) -> MaybeT IO b
    liftFunc''' f = let z = f runMaybeT
                    in MaybeT z

    test''' = runMaybeT go
        where
          go = liftFunc''' $ \run -> testFunc2 $ \x y -> run $ func2 x y

With `liftFunc'''` you can lift a computation that takes a monadic computation that takes any number of arguments.

    liftFunc''' $ \run -> testFunc0 $ run func0
    liftFunc''' $ \run -> testFunc1 $ \x -> run $ func1 x
    liftFunc''' $ \run -> testFunc2 $ \x y -> run $ func2 x y

But it doesn't allow to take more than one computations that return different types. For example,

    testFunc01 :: IO a -> (Int -> IO b) -> IO b
    testFunc01 f g = f >> g 0

    runMaybeT $ liftFunc''' $ \run -> testFunc01 (run $ return 'a') (\n -> run $ return n)

cannot type-check. To make this type-check, we have to make `run` function polymorphic by adding an explicit type signature like:

    liftFunc''' :: ((forall a. MaybeT IO a -> IO (Maybe a)) -> IO (Maybe b)) -> MaybeT IO b

Note that you need to enable RankNTypes extension.
