# Open the door with types, part 1

Imagine you have a door. You can open, close, lock, unlock and knock the door. But there are some preconditions.

1. To open the door, it must be closed
2. To close the door, it must be opened
3. To lock the door, it must be closed
4. To unlock the door, it must be locked
5. To knock the door, it must be closed or locked

When you lock the door, you'll lock it with a key, and you can unlock the door with the same key.

Let's express these ideas in code.

```
module Door
    ( Door
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Text (Text)
```

First, we need `State` representing the current status of a door, and `Door` representing a door itself.

```
data State = Opened | Closed | Locked Text deriving (Show, Eq)

data Door = Door {
    name :: Text,
    state :: State
} deriving (Show, Eq)
```

Next, we have a function to create a locked door.

```
makeLocked :: Text -> Text -> Door
makeLocked name key = Door name $ Locked key
```

Then, we have some functions to open, close, lock, unlock and knock a door.

```
open :: Door -> Maybe Door
open door@(Door _ Closed) = Just $ door { state = Opened }
open _ = Nothing

close :: Door -> Maybe Door
close door@(Door _ Opened) = Just $ door { state = Closed }
close _ = Nothing

lock :: Text -> Door -> Maybe Door
lock key door@(Door _ Closed) = Just $ door { state = Locked key }
lock _ _ = Nothing

unlock :: Text -> Door -> Maybe (Either Door Door)
unlock key door@(Door _ (Locked lockedKey))
    | key == lockedKey = Just $ Right $ door { state = Closed }
    | otherwise = Just $ Left door
unlock _ _ = Nothing

knock :: Door -> Maybe Door
knock door@(Door _ Closed) = Just door
knock door@(Door _ (Locked _)) = Just door
knock _ = Nothing
```

Now, let's write a function to force open a door with these primitive functions.

```
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (( Door -> Maybe Door
forceOpen key door =
    case knock door of
      Just door ->
          case unlock key door of
            Just (Right closedDoor) ->
                case open closedDoor of
                  Just openedDoor -> Just openedDoor
                  Nothing -> error "Must not happen"
            Just (Left _) -> Nothing
            Nothing ->
                case open door of
                  Just openedDoor -> Just openedDoor
                  Nothing -> error "Must not happen"
      Nothing -> Just door
```

As you can see, this function knows something which is not represented by the types. For example, it knows that a door must be opened if you fail to knock it. It knows that it'll get a closed door when it successfully unlocks a door.

These things aren't expressed by types, but expressed as a runtime `state` value.

In this series, I'll try to express these ideas using types to make it safer.
