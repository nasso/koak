{-# LANGUAGE ApplicativeDo #-}

module Args
  ( Args (..),
    OutputMode (..),
    Stage (..),
    parseArgs,
  )
where

import Koa.Analyser
import Koa.Compiler
import Koa.Parser
import Options.Applicative

-- | Command line arguments
data Args = Args
  { -- | Output mode
    argOutputMode :: OutputMode,
    -- | Stages to run
    argStage :: Stage,
    -- | Output file path
    argOutputPath :: Maybe FilePath,
    -- | Parser configuration
    argParserConfig :: ParserConfig,
    -- | Analyser configuration
    argAnalyserConfig :: AnalyserConfig,
    -- | Compiler configuration
    argCompilerConfig :: CompilerConfig
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
    Parse FilePath
  | -- | Parser + type checker
    Check FilePath
  | -- | Parser + type checker + compiler
    Compile FilePath
  deriving (Eq, Show)

parseArgs :: IO Args
parseArgs = execParser opts

-- | Command line arguments parser
opts :: ParserInfo Args
opts =
  info
    ( Args <$> outputMode <*> stage <*> outputFile
        <*> parserCfg
        <*> analyserCfg
        <*> compilerCfg <**> helper
    )
    $ fullDesc
      <> header "koak - The Koa compiler"
      <> progDesc "Compile Koa source files."

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
stage = parseFlag <|> checkFlag <|> compileFlag

-- | "Parse" flag parser
parseFlag :: Parser Stage
parseFlag =
  Parse
    <$> strOption
      ( help "Parse the given file"
          <> metavar "FILE"
          <> long "parse"
          <> short 'p'
      )

-- | "Check" flag parser
checkFlag :: Parser Stage
checkFlag =
  Check
    <$> strOption
      ( help "Type-check the given file"
          <> metavar "FILE"
          <> long "check"
          <> short 't'
      )

-- | "Compile" flag parser
compileFlag :: Parser Stage
compileFlag =
  Compile
    <$> strOption
      ( help "Compile the given file"
          <> metavar "FILE"
          <> long "compile"
          <> short 'c'
      )

-- | Output file argument parser
outputFile :: Parser (Maybe FilePath)
outputFile =
  optional . strOption $
    help "Path to the output file"
      <> metavar "FILE"
      <> long "output"
      <> short 'o'

-- | Parser configuration parser
parserCfg :: Parser ParserConfig
parserCfg = pure ParserConfig

-- | Analyser configuration parser
analyserCfg :: Parser AnalyserConfig
analyserCfg = pure $ AnalyserConfig False

-- | Compiler configuration parser
compilerCfg :: Parser CompilerConfig
compilerCfg = CompilerConfig <$> outputFormatCfg

-- | Output format configuration parser
outputFormatCfg :: Parser OutputFormat
outputFormatCfg =
  flag'
    Assembly
    ( help "Compile only; do not assemble or link."
        <> short 'S'
    )
    <|> pure NativeObject
