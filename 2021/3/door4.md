# Open the door with types, part 4

To avoid passing `SingState` explicitly, you can define a type class whose method returns `SingState` from `State`.

```
class SingStateI (state :: State) where
    singState :: SingState state

instance SingStateI 'Opened where
    singState = SOpened
instance SingStateI 'Closed where
    singState = SClosed
instance SingStateI 'Locked where
    singState = SLocked
```

You no longer need to pass `SingState` explicity to `SomeDoor`.

```
data SomeDoor = forall state. SingStateI state => SomeDoor (Door state)
```

`forceOpen` will have `SingStateI` as its context instead of taking `SingState` explicity.

```
forceOpen :: forall state. SingStateI state => Text -> Door state -> Maybe (Door 'Opened)
forceOpen key door =
    case singState @state of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     Right closedDoor -> Just $ open closedDoor
                     Left _ -> Nothing

forceOpenSomeDoor :: Text -> SomeDoor -> Maybe (Door 'Opened)
forceOpenSomeDoor key (SomeDoor door) = forceOpen key door
```

Now we can write a type-safe door, but writing `State`, `SingState` and `SingStateI` manually every time is troublesome. I'll use [`singletons`](https://hackage.haskell.org/package/singletons) to define them automatically in the next post.
