# Serializing and deserializing free monads

We sometimes use free monads to express programs. Let's take an example, first.

We have this `Free` for free monads.

```
data Free f a = Pure a | Free (f (Free f a))

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Pure a) = Pure (f a)
    fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
    pure :: a -> Free f a
    pure = Pure

    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    Pure f <*> x = fmap f x
    Free g <*> x = Free (fmap (<*> x) g)

instance Functor f => Monad (Free f) where
    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Pure a >>= f = f a
    Free x >>= f = Free (fmap (>>= f) x)

foldFree :: (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree f (Free as) = f as >>= foldFree f
```

We'll use this `Command` as our commands. It supports reading a line and printing a line.

```
data Command a where
    GetLine :: Command String
    PutLine :: String -> Command ()
```

You can use `Coyoneda` to make `Command` a `Functor`, and use `Coyoneda Command` as a functor for `Free` to get `Program`.

```
type Program a = Free (Coyoneda Command) a
```

The basic operations this program supports are `getLineP` and `putLineP`.

```
getLineP :: Program String
getLineP = Free (Coyoneda Pure GetLine)

putLineP :: String -> Program ()
putLineP line = Free (Coyoneda Pure (PutLine line))
```

Then, you can combine them to write your program.

```
program :: Program ()
program = do
    putLineP "Enter: "
    line <- getLineP
    putLineP ("You entered: " ++ line)
```

Once you've defined `runCommand` to run `Command` in `IO`, you can evaluate your programs using `runProgram`.

```
runCommand :: Command a -> IO a
runCommand GetLine = getLine
runCommand (PutLine line) = putStrLn line
```

```
runProgram :: Program a -> IO a
runProgram = foldFree (lowerCoyoneda . hoistCoyoneda runCommand)
```

I run the program in `IO`, but you can run your programs in other monads in the same way by converting `Command a` to `m a` for any `Monad m`.

Now, let's think about serializing and deserializing your programs. When you look at `program` above, you'll notice that you can use any functions in it. It means you can serialize and deserialize any functions if you can serialize and deserialize any of those programs, which sounds almost impossible.

Then, what kind of programs can we serialize and deserialize? When we serialize a program, we'll embed all the possible results. For example, when the first command returns `Bool` (two options) and the second command returns `Maybe Bool` (three options), we have six options. We use `Bounded` typeclass to express this idea. Of course, it's not practical to serialize programs that use `Int`, for example, even though it's an instance of `Bounded`. `Int` has too many options to be serialized.

Instead of using the `Command` above which uses `String` as a result type in `GetLine`, we'll define another `Command`.

```
data LineLength = Short | Medium | Long deriving (Show, Read, Eq, Enum, Bounded)

data Command a where
    GetLineLength :: Command LineLength
    PutLine :: String -> Command ()

runCommand :: Command a -> IO a
runCommand GetLineLength = do
    line <- getLine
    return $ case length line of
        n | n < 10 -> Short
          | n < 20 -> Medium
          | otherwise -> Long
runCommand (PutLine line) = putStrLn line
```

As you can see, it now use `LineLength` which only have three options as a return type. Also, we'll define a type-restricted `Coyoneda`.

```
data Action a = forall r. (Enum r, Bounded r) => Action (Command r) (r -> a)

deriving instance Functor Action
```

`Action` is `Coyoneda Command` with its parameter constrained with `Enum` and `Bounded`. You can write your programs with these `Command` and `Action`.

```
type Program a = Free Action a

runProgram :: Program a -> IO a
runProgram = foldFree interpret
  where
    interpret :: Action a -> IO a
    interpret (Action command f) = f <$> runCommand command

getLineLengthP :: Program LineLength
getLineLengthP = Free (Action GetLineLength Pure)

putLineP :: String -> Program ()
putLineP line = Free (Action (PutLine line) Pure)

program :: Program ()
program = do
    lineLength <- getLineLengthP
    case lineLength of
        Short -> putLineP "You entered a short line."
        Medium -> do putLineP "You entered a medium line."
                     nextLineLength <- getLineLengthP
                     putLineP $ "You entered a " ++ show nextLineLength ++ " line."
        Long -> putLineP "You entered a long line."
```

Making it serializable is relatively easy. You need to make `Command` and `Action` instances of `Show`.

```
deriving instance Show (Command a)

instance (Show a) => Show (Action a) where
    show (Action command next) = show command ++ " " ++ show [ show (next r) | r <- [minBound .. maxBound] ]
```

When you look at the `Show` instance of `Action`, you'll find that it serializes its command itself, then serializes all the possibilities (`[minBound .. maxBound]`) of the following action.

Note that we've already defined `Show` instance of `Free f a` above.

```
deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
```

It serializes the program above (`show program`) like this.

```
"Free GetLineLength [\"Free PutLine \\\"You entered a short line.\\\" [\\\"Pure ()\\\"]\",\"Free PutLine \\\"You entered a medium line.\\\" [\\\"Free GetLineLength [\\\\\\\"Free PutLine \\\\\\\\\\\\\\\"You entered a Short line.\\\\\\\\\\\\\\\" [\\\\\\\\\\\\\\\"Pure ()\\\\\\\\\\\\\\\"]\\\\\\\",\\\\\\\"Free PutLine \\\\\\\\\\\\\\\"You entered a Medium line.\\\\\\\\\\\\\\\" [\\\\\\\\\\\\\\\"Pure ()\\\\\\\\\\\\\\\"]\\\\\\\",\\\\\\\"Free PutLine \\\\\\\\\\\\\\\"You entered a Long line.\\\\\\\\\\\\\\\" [\\\\\\\\\\\\\\\"Pure ()\\\\\\\\\\\\\\\"]\\\\\\\"]\\\"]\",\"Free PutLine \\\"You entered a long line.\\\" [\\\"Pure ()\\\"]\"]"
```

You can see what it does by indenting it and adding some comments.

```
"Free GetLineLength
  [ -- One item for each option of lineLength
    -- Short
    \"Free PutLine \\\"You entered a short line.\\\"
      [ -- Only one option here
         \\\"Pure ()\\\"
      ]\",
    -- Medium
    \"Free PutLine \\\"You entered a medium line.\\\"
      [
        \\\"Free GetLineLength
          [ -- One item for each option of nextLineLength
            -- Short
            \\\\\\\"Free PutLine \\\\\\\\\\\\\\\"You entered a Short line.\\\\\\\\\\\\\\\"
              [ -- Only one option here
                \\\\\\\\\\\\\\\"Pure ()\\\\\\\\\\\\\\\"
              ]\\\\\\\",
            -- Medium
            \\\\\\\"Free PutLine \\\\\\\\\\\\\\\"You entered a Medium line.\\\\\\\\\\\\\\\"
              [ -- Only one option here
                \\\\\\\\\\\\\\\"Pure ()\\\\\\\\\\\\\\\"
              ]\\\\\\\",
            -- Long
            \\\\\\\"Free PutLine \\\\\\\\\\\\\\\"You entered a Long line.\\\\\\\\\\\\\\\"
              [ -- Only one option here
                \\\\\\\\\\\\\\\"Pure ()\\\\\\\\\\\\\\\"
              ]\\\\\\\"
          ]\\\"
        ]\",
    -- Long
    \"Free PutLine \\\"You entered a long line.\\\"
      [ -- Only one option here
        \\\"Pure ()\\\"
      ]\"
  ]"
```

Deserialization is more complex. First we need to support parsing a command, but we cannot have a function `readCommand :: String -> Command a`. It returns `Command LineLength` when you read `"GetLineLength"`, and it returns `Command ()` when you read `"PutLine \"sample\""`. So this function cannot be polymorphic. We need an existential type to wrap `Command` to make it polymorphic.

```
data SomeCommand = forall a. (Show a, Enum a, Bounded a) => SomeCommand (Command a)
```

Then, we can write a function to parse a command and return `SomeCommand`. `readCommand` returns `Maybe (SomeCommand, String)` to return `Nothing` when it fails, and return the rest of the input with a parsed command.

```
readCommand :: String -> Maybe (SomeCommand, String)
readCommand s = case lex s of
    [("GetLineLength", rest)] -> Just (SomeCommand GetLineLength, rest)
    [("PutLine", rest)] -> case lex rest of
        [(line, rest')] -> Just (SomeCommand (PutLine line), rest')
        _ -> Nothing
    _ -> Nothing
```

Now, you can write `readProgram` using `readCommand`.

```
readProgram :: (Read a) => String -> Maybe (Program a)
readProgram s = case lex s of
    [("Pure", rest)] -> Just (Pure $ read rest)
    [("Free", rest)] -> case readCommand rest of
        Just (SomeCommand command, rest') -> case readPrograms rest' of
            Just nextPrograms -> Just (Free (Action command (\r -> nextPrograms !! fromEnum r)))
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

readPrograms :: (Read a) => String -> Maybe [Program a]
readPrograms s = case reads s of
    [(programs, "")] -> sequenceA $ map readProgram programs
    _ -> Nothing
```

You should remember that we serialized all options following the command when we serialized `Action`. When we deserialize it, we read all those options using `readPrograms`, then make it pick the selected option using `fromEnum`.

For example, when `command` in `Action command (\r -> nextPrograms !! from Enum r)` returns `()` (`r` becomes `()`), it'll always pick the first option (because `fromEnum ()` is `0`). It'll pick the second option if `command` returns `Medium` (`r` is `Medium`), and so on.

You'd have noticed this, but `readProgram` returns `(Read a) => Maybe (Program a)` because the program itself doesn't carry any information about its return type. Return values are serialized to `String`, and a caller need to specify its type. For example, you can run it like this.

```
let serializedProgram = show program
let Just deserializedProgram = readProgram serializedProgram
runProgram deserializedProgram :: IO ()
```
