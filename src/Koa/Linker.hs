{-# LANGUAGE TemplateHaskell #-}

module Koa.Linker
  ( linkFilesToExecutable,
  )
where

import Data.FileEmbed
import System.IO
import System.IO.Temp
import System.Process

linkFilesToExecutable :: FilePath -> [FilePath] -> IO ()
linkFilesToExecutable out objs = 
  withSystemTempFile "main.c" $ \p h ->
    hPutStr h entry
      >> hFlush h
      >> rawSystem "cc" ("-o" : out : p : objs)
      >> pure ()
  where
    entry = $(embedStringFile "src/runtime.c") :: String
