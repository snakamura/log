# Fixed point in Strict Haskell, part 2

Now, we [saw `Fix` for $\mu F$, and `Nu` for $\nu F$](./strict_fix1.html). But they're not symmetric. Can we write $\mu F$ whose representation is symmetric to `Nu`? Yes, it's this `Mu`.

```
type Mu :: (Type -> Type) -> Type
newtype Mu f where
  In :: (forall a. (f a -> a) -> a) -> Mu f
```

Just like `Nu` captures anamorphism in itself, `Mu` captures catamorphism in it. `cata` will just run the algebra, and `embed` lifts `f (Mu f)` to `Mu f` by making it run the algebra one more time.

```
cata :: (Functor f) => (f a -> a) -> Mu f -> a
cata alg (In run) = run alg

embed :: (Functor f) => f (Mu f) -> Mu f
embed fmuf = In $ \alg -> alg $ cata alg <$> fmuf
```

`ana` will build catamorphism which folds the structure built by anamorphism.

```
ana :: (Functor f) => (a -> f a) -> a -> Mu f
ana coalg a = In (\alg -> let h x = alg (h <$> coalg x) in h a)
```

`project` runs catamorphism to rebuild the structure as the first element of the pair, and gets the first layer as the second element of the pair, then get the second element of the pair.

```
project :: (Functor f) => Mu f -> f (Mu f)
project @f = snd . cata alg
  where
    alg :: f (Mu f, f (Mu f)) -> (Mu f, f (Mu f))
    alg x = (embed (fst <$> x), fst <$> x)
```

Just like we factored out `apo` from `embed` for `Nu` in the previous post, you can factor out paramorphism from it . Then, `project` can be written with `para`.

```
para :: (Functor f) => (f (Mu f, a) -> a) -> Mu f -> a
para @f @a phi = snd . cata alg
  where
    alg :: f (Mu f, a) -> (Mu f, a)
    alg x = (embed (fst <$> x), phi x)

project :: (Functor f) => Mu f -> f (Mu f)
project = para $ fmap fst
```

Interestingly, we can still build an infinite structure using `Mu` even in Strict Haskell, but cannot evaluate it because evaluating it always run the catamorphism, which never terminates with an infinite structure.

When you put `Mu` and `Nu` together

```
type Mu :: (Type -> Type) -> Type
newtype Mu f where
  In :: (forall a. (f a -> a) -> a) -> Mu f

type Nu :: (Type -> Type) -> Type
data Nu f where
  Out :: (a -> f a) -> a -> Nu f
```

and compare this pair with a pair of `Yoneda` and `Coyoneda`

```
type Yoneda :: (Type -> Type) -> Type -> Type
newtype Yoneda f a where
  Yoneda :: (forall b. ((a -> b) -> f b)) -> Yoneda f a

type Coyoneda :: (Type -> Type) -> Type -> Type
data Coyoneda f a where
  Coyoneda :: (b -> a) -> f b -> Coyoneda f a
```

you'll find some similarities. `Mu f` is an initial algebra represented by this end

$$\int_a ((f a \rightarrow a) \rightarrow a)$$

and `Yoneda f a` is an end of a hom functor.

$$\int_b (Hom(a, b) \rightarrow f b)$$

`Nu f` is a terminal coalgebra represented by this coend

$$\int^a ((a \rightarrow f a) \times a)$$

and `Coyoneda f a` is a coend of a hom functor.

$$\int^b (Hom(b, a) \times f b)$$

But their representations are flipped. `Yoneda f a` is a right adjoint and is represented by an end ($\forall$) as usuall. On the other hand, `Nu f` is a right adjoint but is represented by a coend ($\exists$). The same goes for `Coyoneda f a` and `Mu f`. `Coyoneda f a` is a left adjoint and is represented by a coend ($\exists$) as usuall. `Mu f` is a left adjoint but is represetned by an end ($\forall$).

This is because `Mu` uses the church encoding to represent the fixed point. Just like $\exists_a. a$ can be represented by $\forall_a. (a \rightarrow r) \rightarrow r$ in CPS-style, `Mu` is represented with $\forall$ even though its direct representation (`Fix`) is represented by $\exists$ ($\mu f$ is a coproduct of all $f^n \varnothing $ quantiented by the equivalent relation ($(\displaystyle \coprod_n f^n \varnothing) / \sim$), so its colimit ($\displaystyle \operatorname*{colim}_n f^n \varnothing$) is $\exists_n. f^n \varnothing$).

The same goes for `Nu`. $\forall_a. a$ can be represented by $\exists_a. (a \rightarrow r) \rightarrow r$ in CPS-style. The direct representation of $\nu f$ (again `Fix`) is a product of all $f^n 1$ ($\displaystyle \prod_n f^n 1$), and its limit ($\displaystyle \operatorname*{lim}_n f^n 1$) is $\forall_n. f^n 1$. So it'll be represented with $\exists$ using the church encoding.
