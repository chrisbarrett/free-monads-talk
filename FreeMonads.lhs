[//]: -*- mode: markdown; -*-

Free Monads
===

This module demonstrates a domain specific language (DSL) implemented using
_free monads_ We will write a small language for performing operations on the
filesystem.

Using a free monad allows you to treat any type (which has a certain structure)
into a monad. _Free_ in this sense means "comes for free"--it's a trivial way to
make your type satisfy all the monad laws and thus be an instance of a monad,
but without actually doing anything interesting. The monadic bind on a free
monad simply builds up a nested structure.

By embedding our language in a free monad we are able to use those convenient
monadic operations to build our programs. But critically, these monadic
operations will only build up a data structure _representing_ the program we
want to run--nothing will be executed. We can then take that data structure and
pass it off to different interpreters, which could do anything:

- Execute the actual operations on disk
- Collect the commands called but not do any IO (great for testing)
- Compile our program to C.

Pretty versatile stuff.

Free monads are great if you know ahead of time that you might want to interpret
a program in many different ways. However, you must write some boilerplate
functions and understand the indirection introduced by having separate
interpreters. There is a trade-off to be made here.

This is a literate Haskell file, which you can run in GHC. Let's get those pesky
imports and pragmas out of the way.

> {-# LANGUAGE DeriveFunctor       #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell     #-}
> module FreeMonads where
>
> import           Control.Exception      as Exception
> import           Control.Monad          as Monad
> import           Control.Monad.Free     as Free
> import           Control.Monad.IO.Class
> import           Control.Monad.Logger   as Logger
> import           Data.Monoid
> import           Data.Text              as Text hiding (length)
> import qualified System.Directory       as Directory
> import           System.FilePath        ((</>))


Language Definition
---

First, we define the operations we will support in our language.

> data FileSystem a =
>     ListFiles FilePath ([FilePath] -> a)
>   | TemporaryDirectory (FilePath  -> a)
>   | DirectoryExists FilePath (Bool -> a)
>   | CreateDirectory FilePath (() -> a)
>   | DeleteDirectory FilePath (() -> a)
>   deriving (Functor)

Our type must be a Functor to be embedded in the free monad.

For convenience, we define a type alias to make the upcoming type signatures
cleaner.

> type FileSystemF a = Free FileSystem a

Now we define some helper functions to embed our type in a free monad. These are
the functions we will call to write programs in our language. They're basically
boilerplate.

> listFiles :: FilePath -> FileSystemF [FilePath]
> listFiles f = Free.liftF (ListFiles f id)
>
> temporaryDirectory :: FileSystemF FilePath
> temporaryDirectory = Free.liftF (TemporaryDirectory id)
>
> directoryExists :: FilePath -> FileSystemF Bool
> directoryExists f = Free.liftF (DirectoryExists f id)
>
> createDirectory :: FilePath -> FileSystemF ()
> createDirectory f = Free.liftF (CreateDirectory f id)
>
> deleteDirectory :: FilePath -> FileSystemF ()
> deleteDirectory f = Free.liftF (DeleteDirectory f id)

Now that our operations are embedded in Free, we have a monad instance to play
with. We can use monadic bind (`>>=`), do-notation, and all the operations
available on functors, applicatives and monads. We have everything we need to
stitch together a little program in our language.

> program = do
>     tmpDir <- temporaryDirectory
>     before <- listFiles tmpDir
>     let target = tmpDir </> "free-monads-example"
>     exists <- directoryExists target
>     if exists
>       then deleteDirectory target
>       else createDirectory target
>     after <- listFiles tmpDir
>     pure (length before, length after)

This program will either create or delete a directory in your temp files,
depending on whether it already exists.


Interpreters
---

Now that we can write programs in our language, we define interpreters to
actually execute them.

Here's an interpreter that executes operations on disk:

> execFS :: MonadIO io => FileSystem a -> io a
> execFS c =
>     case c of
>       ListFiles path f ->
>           f <$> liftIO (Directory.getDirectoryContents path)
>
>       DirectoryExists path f ->
>           f <$> liftIO checkExistence
>          where
>            checkExistence =
>               -- This lookup can raise an error, depending on user permissions.
>               Directory.doesDirectoryExist path
>                    `catch` (\(e :: Exception.IOException) -> pure False)
>
>       TemporaryDirectory f ->
>           f <$> liftIO Directory.getTemporaryDirectory
>
>       CreateDirectory path f ->
>           f <$> liftIO (Directory.createDirectory path)
>
>       DeleteDirectory path f ->
>           f <$> liftIO (Directory.removeDirectory path)

Here's an interpreter that wraps around another to add logging:

> execFSLogged :: MonadLogger log => (FileSystem a -> m a) -> FileSystem a -> log (m a)
> execFSLogged inner c = do
>     case c of
>       ListFiles {} ->
>           $(logDebug) "Listing files"
>
>       TemporaryDirectory f ->
>           $(logDebug) "Finding temporary directory"
>
>       DirectoryExists path f ->
>           $(logDebug) ("Directory exists? directory=" <> tshow path)
>
>       CreateDirectory path f ->
>           $(logDebug) ("Creating directory: directory=" <> tshow path)
>
>       DeleteDirectory path f ->
>           $(logDebug) ("Deleting directory: directory=" <> tshow path)
>
>     pure (inner c)
>
>   where
>     tshow = Text.pack . show


Execution
---

Now that we have some interpreters, we can run our program!

The action below will execute the operations on disk.

> example1 :: IO ()
> example1 = do
>     (before, after) <- runIO program
>     putStrLn ("before: " <> show before <> " entries")
>     putStrLn ("after: " <> show after <> " entries")
>
> runIO :: MonadIO m => FileSystemF a -> m a
> runIO p = Free.foldFree execFS p

The action below will do the same as above, but also use the logging interpreter
to output logs as commands are run.

> example2 :: IO ()
> example2 = do
>     (before, after) <- runLoggedIO program
>     putStrLn ("before: " <> show before <> " entries")
>     putStrLn ("after: " <> show after <> " entries")
>
> runLoggedIO :: MonadIO m => FileSystemF a -> m a
> runLoggedIO = runStdoutLoggingT . Free.foldFree (Monad.join . execFSLogged execFS)

Summary
---

You've seen how to define a data type to represent a DSL, then wrap it in the
free monad to make working with it more convenient. Then, you saw how to run
programs written in your DSL.
