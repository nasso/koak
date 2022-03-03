module Test.Koa.Parser (parserTests) where

import Test.Koa.Parser.Binop
import Test.Koa.Parser.Let
import Test.Koa.Parser.Loop
import Test.Koa.Parser.Returns
import Test.Tasty

parserTests :: [TestTree]
parserTests =
  [ testGroup "Returns" returnTests,
    testGroup "Binop" binopTests,
    testGroup "Let" letTests,
    testGroup "Loop" loopTests
  ]
