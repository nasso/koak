module Koa.Analyser
  ( AnalyserConfig (..),
    analyseProgram,
  )
where

import Koa.Syntax (Program, ProgramT)

-- | Configuration for the analyser.
data AnalyserConfig = AnalyserConfig
  {
  }
  deriving (Show, Eq)

analyseProgram :: AnalyserConfig -> Program -> Either String ProgramT
analyseProgram = error "unimplemented analyseProgram"
