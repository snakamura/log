# Evaluate lenses by hand

I was wondering how I could get lenses from this definition:

    type Lens' a b = Functor f => (b -> f b) -> a -> f a

so I tried to define a simple lenses and evaluate them by hand. Here are simple lenses and some utilities.

    {-# LANGUAGE Rank2Types #-}

    import Data.Functor
    import Data.Functor.Constant
    import Data.Functor.Identity

    type Lens' a b = Functor f => (b -> f b) -> a -> f a

    view :: Lens' a b -> a -> b
    view l a = getConstant $ l Constant a

    set :: Lens' a b -> b -> a -> a
    --set l b = over l (const b)
    set l b a = runIdentity $ l (Identity . const b) a

    over :: Lens' a b -> (b -> b) -> a -> a
    over l f a = runIdentity $ l (Identity . f) a

    _1 :: Lens' (p, q) p
    _1 g (x, y) = (\x' -> (x', y)) <$> g x

    _2 :: Lens' (p, q) q
    _2 g (x, y) = (\y' -> (x, y')) <$> g y

And `view _1` and `over _1` are evaluated this way.

    view _1 (p, q) = getConstant $ _1 Constant (p, q)
                   -- _1 = \g (x, y) -> (\x' -> (x', y)) <$> g x
                   = getConstant $ (\g (x, y) -> (\x' -> (x', y)) <$> g x) Constant (p, q)
                   -- g = Constant, x = p, y = q
                   = getConstant $ (\x' -> (x', q)) <$> Constant p
                   -- _ <$> Constant p = Constant p
                   = getConstant $ Constant p
                   = p

    over _1 f (p, q) = runIdentity $ _1 (Identity . f) (p, q)
                     -- _1 = \g (x, y) -> (\x' -> (x', y)) <$> g x
                     = runIdentity $ (\g (x, y) -> (\x' -> (x', y)) <$> g x) (Identity . f) (p, q)
                     -- g = Identity . f, x = p, y = q
                     = runIdentity $ (\x' -> (x', q)) <$> (Identity . f) p
                     = runIdentity $ (\x' -> (x', q)) <$> Identity (f p)
                     -- h <$> Identity p = Identity (h p)
                     = runIdentity $ Identity ((\x' -> (x', q)) (f p))
                     = runIdentity $ Identity (f p, q)
                     = (f p, q)
