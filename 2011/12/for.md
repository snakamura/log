# for_ is whenJust

Sometimes, I defined a utility function whenJust as follows:

    whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
    whenJust = flip $ maybe (return ())

and use it like:

    whenJust x $ print

But finally I found that `Data.Foldable.forM_` is equivalent to this function thanks to [Proposal: add Control.Monad.whenJust ∷ (Monad m) ⇒ Maybe α→(α→ m ()) → m ()](http://haskell.1045720.n5.nabble.com/Proposal-add-Control-Monad-whenJust-Monad-m-Maybe-m-m-td5034761.html). Actually, this is obvious because Maybe is a container that may have one element. You can also use `Data.Foldable.for_` for applicative functors. Now you can write:

    for_ x $ print

without defining your whenJust.
