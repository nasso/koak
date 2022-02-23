{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Args
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe
import Koa.Analyser
import Koa.Compiler
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

-- | Parse a program and write the AST to the output file
parse :: FilePath -> App ()
parse p = parsed p >>= outputStr (p -<.> "txt") . show

-- | Parse and type-check a program and write the typed AST to the output file
check :: FilePath -> App ()
check p = checked p >>= outputStr (p -<.> "txt") . show

-- | Parse, type-check and compile a program and write the compiled binary to
-- the output file
compile :: FilePath -> App ()
compile p =
  do
    tast <- checked p
    cfg <- asks argCompilerConfig
    out <-
      outputPath
        ( p -<.> case cfgFormat cfg of
            Assembly -> "ll"
            NativeObject -> "o"
        )
    liftIO $ compileProgramToFile out cfg tast

-- | Parse a file into an AST
parsed :: FilePath -> App Program
parsed p =
  do
    src <- liftIO (readFile p)
    case parseProgram ParserConfig src of
      Left s -> logError (p ++ ":" ++ s) >> throwError ()
      Right ast -> pure ast

-- | Parse and type-check a file into a typed AST
checked :: FilePath -> App ProgramT
checked p =
  do
    cfg <- asks argAnalyserConfig
    ast <- parsed p
    case analyseProgram cfg ast of
      Left s -> logError (p ++ ":\n" ++ show s) >> throwError ()
      Right (tast, warnings) -> tast <$ traverse_ warn (show <$> warnings)
  where
    warn s = logWarn (p ++ ":\n" ++ s)

-- | Write data to the output file
outputStr :: FilePath -> String -> App ()
outputStr def str =
  do
    out <- outputPath def
    liftIO (writeFile out str)

-- | Get the output file path or a default.
outputPath :: FilePath -> App FilePath
outputPath defaultPath = asks (fromMaybe defaultPath . argOutputPath)

-- | Display an error message
logError :: String -> App ()
logError msg = liftIO $ hPutStrLn stderr $ "error: " ++ msg

-- | Display a warning message
logWarn :: String -> App ()
logWarn msg = liftIO $ hPutStrLn stderr $ "warning: " ++ msg
