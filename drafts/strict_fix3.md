# Fixed point in Strict Haskell, part 3

We saw `Fix`, `Mu`, and `Nu` in the previous [two](./strict_fix1.html) [posts](./strict_fix1.html). When you put `Mu` and `Nu` together

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

But their representations are somewhat flipped. `Yoneda f a` is a right adjoint and is represented by an end ($\forall$) as usuall. On the other hand, `Nu f` is a right adjoint but is represented by a coend ($\exists$). The same goes for `Coyoneda f a` and `Mu f`. `Coyoneda f a` is a left adjoint and is represented by a coend ($\exists$) as usuall. `Mu f` is a left adjoint but is represetned by an end ($\forall$).

This is because `Mu` uses the church encoding to represent the fixed point. Just like $\exists_a. a$ can be represented by $\forall_a. (a \rightarrow r) \rightarrow r$ in CPS-style, `Mu` is represented with $\forall$ even though its direct representation (`Fix`) is represented by $\exists$ ($\mu f$ is a coproduct of all $f^n \varnothing $ quantiented by the equivalent relation ($(\displaystyle \coprod_n f^n \varnothing) / \sim$), so its colimit ($\displaystyle \operatorname*{colim}_n f^n \varnothing$) is $\exists_n. f^n \varnothing$).

The same goes for `Nu`. $\forall_a. a$ can be represented by $\exists_a. (a \rightarrow r) \rightarrow r$ in CPS-style. The direct representation of $\nu f$ (again `Fix`) is a product of all $f^n 1$ ($\displaystyle \prod_n f^n 1$), and its limit ($\displaystyle \operatorname*{lim}_n f^n 1$) is $\forall_n. f^n 1$. So it'll be represented with $\exists$ using the church encoding.
