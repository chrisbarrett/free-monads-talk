{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Talk where

import           Control.Monad          as Monad
import           Control.Monad.Free     as Free
import           Control.Monad.IO.Class
import           Control.Monad.Logger   as Logger
import           Data.Monoid
import           Data.Text              as Text
import qualified System.Directory       as Directory
import           System.FilePath        ((</>))


-- * File IO DSL

data FileSystem a =
    ListFiles FilePath ([FilePath] -> a)
  | CreateDirectory FilePath (() -> a)
  | DeleteDirectory FilePath (() -> a)
  deriving (Functor)

type FileSystemF a = Free FileSystem a

listFiles :: FilePath -> FileSystemF [FilePath]
listFiles f = Free.liftF (ListFiles f id)

createDirectory :: FilePath -> FileSystemF ()
createDirectory f = Free.liftF (CreateDirectory f id)

deleteDirectory :: FilePath -> FileSystemF ()
deleteDirectory f = Free.liftF (DeleteDirectory f id)


-- * File IO interpreters

execFS :: MonadIO io => FileSystem a -> io a
execFS c =
    case c of
      ListFiles path f ->
          f <$> liftIO (Directory.getDirectoryContents path)

      CreateDirectory path f ->
          f <$> liftIO (Directory.createDirectory path)

      DeleteDirectory path f ->
          f <$> liftIO (Directory.removeDirectory path)


execFSLogged :: MonadLogger log => (FileSystem a -> m a) -> FileSystem a -> log (m a)
execFSLogged inner c = do
    case c of
      ListFiles {} ->
          $(logDebug) "Listing files"

      CreateDirectory path f ->
          $(logDebug) ("Creating directory: directory=" <> tshow path)

      DeleteDirectory path f ->
          $(logDebug) ("Deleting directory: directory=" <> tshow path)

    pure (inner c)

  where
    tshow = Text.pack . show


program = do
    a <- listFiles "/Users/chrisb/example"
    _ <- createDirectory "/Users/chrisb/example/hello"
    b <- listFiles "/Users/chrisb/example"
    _ <- deleteDirectory ("/Users/chrisb/example/hello")
    c <- listFiles "/Users/chrisb/example"
    pure (a, b, c)


example1 :: IO ()
example1 = do
    (a, b, c) <- run program
    print a
    print b
    print c
  where
    run p = Free.iterM (Monad.join . execFS) p


example2 = do
    (a, b, c) <- run program
    print a
    print b
    print c
  where
    run =
      runStdoutLoggingT . Free.iterM
          ( Monad.join
          . Monad.join
          . execFSLogged execFS
          )
