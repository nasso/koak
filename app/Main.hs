{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Args
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.FileEmbed
import Data.Foldable
import Data.Maybe
import Koa.Analyser
import Koa.Compiler
import Koa.Parser
import Koa.Syntax
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

newtype App a = App {runApp :: ReaderT Args (ExceptT () IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Args,
      MonadError (),
      MonadMask,
      MonadCatch
    )

-- | Required by MonadMask and MonadCatch
instance MonadThrow App where
  throwM = App . throwM

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
      Codegen p -> codegen p
      Link p -> link p

-- | Parse a program and write the AST to the output file
parse :: FilePath -> App ()
parse p = parsed p >>= outputStr (p -<.> "txt") . show

-- | Parse and type-check a program and write the typed AST to the output file
check :: FilePath -> App ()
check p = checked p >>= outputStr (p -<.> "txt") . show

-- | Parse, type-check and IR codegen a program and write the IR to the output
-- file
codegen :: FilePath -> App ()
codegen p =
  do
    tast <- checked p
    cfg <- asks argCompilerConfig
    out <- outputPath (p -<.> "ll")
    liftIO $ compileProgramToFile out (cfg {cfgFormat = Assembly}) tast

-- | Parse, type-check and compile a program and write the compiled binary to
-- the output file
compile :: FilePath -> App ()
compile p =
  do
    tast <- checked p
    cfg <- asks argCompilerConfig
    out <- outputPath (p -<.> "o")
    liftIO $ compileProgramToFile out (cfg {cfgFormat = NativeObject}) tast

-- | Compile all input files if necessary and link them into an executable
link :: [FilePath] -> App ()
link ps = compileAllAndLink ps []

-- | Parse, type-check and compile all input files in temporary files if
-- necessary and link them into an executable
compileAllAndLink :: [FilePath] -> [FilePath] -> App ()
compileAllAndLink [] objs = withSystemTempFile "main.c" $ \p h ->
  do
    _ <- liftIO $ hPutStr h entry
    _ <- liftIO $ hFlush h
    out <- outputPath "a.out"
    _ <- liftIO $ rawSystem "gcc" (p : objs ++ ["-o", out])
    pure ()
  where
    entry = $(embedStringFile "src/entry.c") :: String
compileAllAndLink (p : ps) objs = case ".koa" `isExtensionOf` p of
  False -> compileAllAndLink ps (p : objs)
  True -> withSystemTempFile "koa.o" $ \out _ ->
    do
      tast <- checked p
      cfg <- asks argCompilerConfig
      liftIO $ compileProgramToFile out (cfg {cfgFormat = NativeObject}) tast
      compileAllAndLink ps (out : objs)

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
