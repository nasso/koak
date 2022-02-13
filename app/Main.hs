{-# LANGUAGE ApplicativeDo #-}

module Main where

import Options.Applicative

-- | Command line options
data Options = Options
  { -- | Output mode
    optOutputMode :: OutputMode,
    -- | Stages to run
    optStage :: Stage,
    -- | Input file paths
    optInputPaths :: [FilePath],
    -- | Output file path
    optOutputPath :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | An output mode
data OutputMode
  = -- | Normal output
    Normal
  | -- | Verbose output
    Verbose
  | -- | Quiet output
    Quiet
  deriving (Eq, Show)

-- | A stage
data Stage
  = -- | Parser only
    Parse
  | -- | Parser + type checker
    Check
  | -- | Parser + type checker + compiler
    Compile
  | -- | Parser + type checker + compiler + linker
    Link
  deriving (Eq, Show)

-- | The main entry point
main :: IO ()
main = execParser opts >>= print

-- | Options parser
opts :: ParserInfo Options
opts =
  info
    (Options <$> outputMode <*> stage <*> inputFiles <*> outputFile <**> helper)
    ( fullDesc
        <> header "koak - The Koa compiler"
        <> progDesc "Compile and link Koa source files."
    )

-- | Output flags parser
outputMode :: Parser OutputMode
outputMode = verboseFlag <|> quietFlag <|> pure Normal

-- | "Verbose" flag parser
verboseFlag :: Parser OutputMode
verboseFlag =
  flag' Verbose $
    help "Verbose output"
      <> long "verbose"
      <> short 'v'

-- | "Quiet" flag parser
quietFlag :: Parser OutputMode
quietFlag =
  flag' Quiet $
    help "Quiet output"
      <> long "quiet"
      <> short 'q'

-- | Stage flags parser
stage :: Parser Stage
stage = parseFlag <|> checkFlag <|> compileFlag <|> pure Link

-- | "Parse" flag parser
parseFlag :: Parser Stage
parseFlag =
  flag' Parse $
    help "Parse the input files"
      <> long "parse"
      <> short 'p'

-- | "Check" flag parser
checkFlag :: Parser Stage
checkFlag =
  flag' Check $
    help "Type check the input files"
      <> long "check"
      <> short 't'

-- | "Compile" flag parser
compileFlag :: Parser Stage
compileFlag =
  flag' Compile $
    help "Compile the input files without linking them"
      <> long "compile"
      <> short 'c'

-- | Input file paths parser
inputFiles :: Parser [FilePath]
inputFiles =
  some . strArgument $
    help "Input files"
      <> metavar "FILES..."

-- | Output file argument parser
outputFile :: Parser (Maybe FilePath)
outputFile =
  optional . strOption $
    help "Output file"
      <> metavar "FILE"
      <> long "output"
      <> short 'o'
