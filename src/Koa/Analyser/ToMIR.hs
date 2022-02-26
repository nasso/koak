module Koa.Analyser.ToMIR where

import Koa.Analyser.Monad
import qualified Koa.Syntax.HIR as HIR
import qualified Koa.Syntax.MIR as MIR

program :: HIR.ProgramT -> Analyser MIR.Program
program = undefined
