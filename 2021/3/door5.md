# Open the door with types, part 5

In [the previous post](https://snak.tumblr.com/post/646361617047650304/open-the-door-with-types-part-4), I implemented `SingState` and `SingStateI` manually.

With the help of [singletons](https://hackage.haskell.org/package/singletons), you no longer need to implement them manually.

```
{-# LANGUAGE DataKinds,
             GADTs,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TemplateHaskell,
             TypeFamilies
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked)
    , SState(SOpened, SClosed, SLocked)
    , SomeDoor(SomeDoor)
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Kind (Constraint, Type)
import Data.Singletons.TH
import Data.Text (Text)

singletons [d|
    data State = Opened | Closed | Locked
  |]

data Door :: State -> Type where
    OpenedDoor :: Text -> Door 'Opened
    ClosedDoor :: Text -> Door 'Closed
    LockedDoor :: Text -> Text -> Door 'Locked
deriving instance Show (Door state)

data SomeDoor = forall state. SingI state => SomeDoor (Door state)
```

You need to enable some language extensions and import `Data.Singletons.TH` to do this. As you can see, I put the declaration of `State` in `singletons` template function.

`singletons` generates `Sing` and `SingI` for `State` (actually, it generates more but you can ignore them here). `Sing` is a generalized version of `SingState` and `SingI` is a generalized version of `SingStateI`. So you can write `forceOpen` by replacing `SingStateI` to `SingI`, and `singState` to `sing`.

```
forceOpen :: forall state. SingI state => Text -> Door state -> Maybe (Door 'Opened)
forceOpen key door =
    case sing @state of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     Right closedDoor -> Just $ open closedDoor
                     Left _ -> Nothing

forceOpenSomeDoor :: Text -> SomeDoor -> Maybe (Door 'Opened)
forceOpenSomeDoor key (SomeDoor door) = forceOpen key door
```
