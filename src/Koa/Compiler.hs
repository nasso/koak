module Koa.Compiler
  ( CompilerConfig (..),
    CompilationTarget (..),
    compileProgram,
  )
where

import Data.Word (Word8)
import Koa.Syntax (ProgramT)

-- | Configuration for the compiler.
newtype CompilerConfig = CompilerConfig
  { target :: CompilationTarget
  }
  deriving (Show, Eq)

-- | A compilation target.
data CompilationTarget = Assembly | Bitcode deriving (Show, Eq)

-- | Compile a program.
compileProgram :: CompilerConfig -> ProgramT -> [Word8]
compileProgram = error "unimplemented compileProgram"
