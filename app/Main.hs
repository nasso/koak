{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Args
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Koa.Analyser
import Koa.Parser
import Koa.Syntax
import System.Exit
import System.FilePath
import System.IO

newtype App a = App {runApp :: ReaderT Args (ExceptT () IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Args,
      MonadError ()
    )

-- | The main entry point
main :: IO ()
main =
  do
    args <- parseArgs
    exit <- runExceptT (runReaderT (runApp app) args)
    case exit of
      Left _ -> exitFailure
      Right _ -> exitSuccess

-- | The application logic
app :: App ()
app =
  do
    stage <- asks argStage
    case stage of
      Parse p -> parse p
      Check p -> check p
      Compile p -> compile p
      Link ps -> link ps

-- | Parse a program and write the AST to the output file
parse :: FilePath -> App ()
parse p = parsed p >>= outputStr (p -<.> "txt") . show

-- | Parse and type-check a program and write the typed AST to the output file
check :: FilePath -> App ()
check p = checked p >>= outputStr (p -<.> "txt") . show

-- | Parse, type-check and compile a program and write the compiled binary to
-- the output file
compile :: FilePath -> App ()
compile = error "unimplemented compile"

-- | Link a set of compiled programs and write the resulting binary to the
-- output file
link :: [FilePath] -> App ()
link = error "unimplemented link"

-- | Parse a file into an AST
parsed :: FilePath -> App Program
parsed p =
  do
    src <- liftIO (readFile p)
    case parseProgram ParserConfig src of
      Left s -> logError (p ++ ":\n" ++ s) >> throwError ()
      Right ast -> pure ast

-- | Parse and type-check a file into a typed AST
checked :: FilePath -> App ProgramT
checked p =
  do
    cfg <- analyserConfig
    ast <- parsed p
    case analyseProgram cfg ast of
      Left s -> logError (p ++ ":\n" ++ show s) >> throwError ()
      Right (tast, warnings) -> tast <$ traverse_ warn (show <$> warnings)
  where
    warn s = logWarn (p ++ ":\n" ++ s)

-- | The analysis configuration
analyserConfig :: App AnalyserConfig
analyserConfig = pure $ AnalyserConfig False

-- | Write data to the output file
outputStr :: FilePath -> String -> App ()
outputStr def str =
  do
    out <- asks (fromMaybe def . argOutputPath)
    liftIO (writeFile out str)

-- | Display an error message
logError :: String -> App ()
logError msg = liftIO $ hPutStrLn stderr $ "error: " ++ msg

-- | Display a warning message
logWarn :: String -> App ()
logWarn msg = liftIO $ hPutStrLn stderr $ "warning: " ++ msg
