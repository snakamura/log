# Open the door with types, part 3

Now, I'm going to use one type for a door but make it indexed by a phantom type to distinguish its state.

```
{-# LANGUAGE DataKinds,
             GADTs,
             StandaloneDeriving,
             TypeFamilies
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked)
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
    , SingState(SOpened, SClosed, SLocked)
    , SomeDoor(SomeDoor)
) where

import Data.Kind (Constraint, Type)
import Data.Text (Text)

data State = Opened | Closed | Locked

data Door :: State -> Type where
    OpenedDoor :: Text -> Door 'Opened
    ClosedDoor :: Text -> Door 'Closed
    LockedDoor :: Text -> Text -> Door 'Locked
deriving instance Show (Door state)
```

As you can see, `Door` is now a type indexed by `State`. So it can be one of `Door 'Opened`, `Door 'Closed` or `Door 'Locked`.

Functions working on a door are pretty much similar to ones in [the previous post](https://snak.tumblr.com/post/645990203514896384/open-the-door-with-types-part-2).

```
name :: Door state -> Text
name (OpenedDoor name) = name
name (ClosedDoor name) = name
name (LockedDoor name _) = name

makeLocked :: Text -> Text -> Door 'Locked
makeLocked name key = LockedDoor name key

open :: Door 'Closed -> Door 'Opened
open (ClosedDoor name) = OpenedDoor name

close :: Door 'Opened -> Door 'Closed
close (OpenedDoor name) = ClosedDoor name

lock :: Text -> Door 'Closed -> Door 'Locked
lock key (ClosedDoor name) = LockedDoor name key

unlock :: Text -> Door 'Locked -> Either (Door 'Locked) (Door 'Closed)
unlock key lockedDoor@(LockedDoor name lockedKey)
    | key == lockedKey = Right $ ClosedDoor name
    | otherwise = Left lockedDoor

type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

knock :: Knockable state => Door state -> Door state
knock = id
```

To implement `knock`, I used a type family instead of using a type class, but they're almost identical here. With a type class, you can give each instance a different implemention, while you can only use one implementation with a type family. If what you want is to limit the types you can pass to a function, using a type family would fit better.

Now, I'll define a singleton of `State` so that we can express these types in the value world.

```
data SingState :: State -> Type where
    SOpened :: SingState 'Opened
    SClosed :: SingState 'Closed
    SLocked :: SingState 'Locked
```

Then, I'll define `SomeDoor` existential type which can have one of `Door 'Opened`, `Door 'Closed` or `Door 'Locked`.

```
data SomeDoor = forall state. SomeDoor (SingState state) (Door state)
```

Now, let's define `forceOpen`.


```
{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings
#-}

import Data.Either (fromRight)
import Data.Text (Text)
import Door

forceOpen :: Text -> SingState state -> Door state -> Maybe (Door 'Opened)
forceOpen key singState door =
    case singState of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     Right closedDoor -> Just $ open closedDoor
                     Left _ -> Nothing

forceOpenSomeDoor :: Text -> SomeDoor -> Maybe (Door 'Opened)
forceOpenSomeDoor key (SomeDoor singState door) = forceOpen key singState door
```

As you can see, you no longer need to define `forceOpen` as a method of a type class, but directly switch on its type.

This works because when `singState` is `SOpened`, for example, `state` must be `'Opened`, so `door` must be `Door 'Opened`.

It's still a bit annoying that you need to pass both `SingState state` and `Door state` to this function. I'll try fixing it in the next post.
