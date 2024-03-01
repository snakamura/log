# Implementing length-indexed vector using dependent types in Idris

Now that we've implemented `Optional` in [the previous post](https://snak.tumblr.com/post/666931568388997120/implementing-maybe-using-dependent-types-in-idris), let's try implementing a length-indexed vector based on [the Haskell implementation](https://snak.tumblr.com/post/662906879044976640/implementing-length-indexed-vector-using-dependent).

It's very similar to [the Haskell with singletons version](https://snak.tumblr.com/post/662906879044976640/implementing-length-indexed-vector-using-dependent), but there're some differences. First, we need to implement `sameNat` manually.

```
sameNat : (n : Nat) -> (m : Nat) -> Maybe (n = m)
sameNat Z Z = Just Refl
sameNat (S n) (S m) = case sameNat n m of
                        Just e => Just (cong S e)
                        Nothing => Nothing
sameNat _ _ = Nothing
```

This is similar to `sameSNat` in [the Haskell version without singletons](https://snak.tumblr.com/post/662857073524113408/implementing-length-indexed-vector-using-dependent), but you need to make it clear that `S n = S m` when `n = m` using `cong`.

Note that the parameters of `cong` changed in Idris 2, so you need to change `Just (cong S e)` to `Just (cong e)` to run this code with Idris 1.

Other than that, it's just simple translation from Haskell to Idris.

```
data Vec : Nat -> Type -> Type where
    VNil : Vec 0 a
    VCons : a -> Vec len a -> Vec (S len) a

vec : List a -> (len ** Vec len a)
vec [] = (0 ** VNil)
vec (x::xs) = case vec xs of
                (_ ** v) => (_ ** VCons x v)

sum3 : Monoid a => Vec 3 a -> a
sum3 (VCons a1 (VCons a2 (VCons a3 VNil))) = a1  a2  a3

trySum3 : Monoid a => List a -> Maybe a
trySum3 xs = case vec xs of
               (S (S (S Z)) ** v) => Just (sum3 v)
               _ => Nothing

vzip : Vec len a -> Vec len b -> Vec len (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) $ vzip xs ys

tryZip : List a -> List b -> Maybe (len ** Vec len (a, b))
tryZip xs ys = case (vec xs, vec ys) of
                 ((lenX ** vx), (lenY ** vy)) =>
                   case sameNat lenX lenY of
                     Just Refl => Just (_ ** vzip vx vy)
                     _ => Nothing
```
