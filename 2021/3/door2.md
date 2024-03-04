# Open the door with types, part 2

The most obvious approach is to use a different type for each state.

```
module Door
    ( OpenedDoor
    , ClosedDoor
    , LockedDoor
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Text (Text)

data Door = Door {
    name :: Text
} deriving (Show, Eq)

data OpenedDoor = OpenedDoor Door deriving (Show, Eq)
data ClosedDoor = ClosedDoor Door deriving (Show, Eq)
data LockedDoor = LockedDoor Door Text deriving (Show, Eq)
```

Now `open` function only takes `OpenedDoor`, and `lock` takes `ClosedDoor`. These functions no longer fail, so they no longer need to return `Maybe`.

```
makeLocked :: Text -> Text -> LockedDoor
makeLocked name key = LockedDoor (Door name) key

open :: ClosedDoor -> OpenedDoor
open (ClosedDoor door) = OpenedDoor door

close :: OpenedDoor -> ClosedDoor
close (OpenedDoor door) = ClosedDoor door

lock :: Text -> ClosedDoor -> LockedDoor
lock key (ClosedDoor door) = LockedDoor door key

unlock :: Text -> LockedDoor -> Either LockedDoor ClosedDoor
unlock key lockedDoor@(LockedDoor door lockedKey)
    | key == lockedKey = Right $ ClosedDoor door
    | otherwise = Left lockedDoor
```

Since we use different types, we need to make `knock` a method of a type class and make `ClosedDoor` and `LockedDoor` its instances.

```
class KnockableDoor door where
    knock :: door -> door
    knock = id
instance KnockableDoor ClosedDoor
instance KnockableDoor LockedDoor
```

The function to force open a door also becomes a type class method.

```
{-# LANGUAGE OverloadedStrings #-}

import Data.Either (fromRight)
import Data.Text (Text)
import Door

class ForceOpenableDoor door where
    forceOpen :: Text -> door -> Maybe OpenedDoor

instance ForceOpenableDoor OpenedDoor where
    forceOpen _ = Just
instance ForceOpenableDoor ClosedDoor where
    forceOpen _ = Just . open . knock
instance ForceOpenableDoor LockedDoor where
    forceOpen key lockedDoor
        | Right closedDoor
```
