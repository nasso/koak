module Test.Koa.Parser (parserTests) where

import Test.Koa.Parser.Assignment
import Test.Koa.Parser.Binop
import Test.Koa.Parser.Functions
import Test.Koa.Parser.Let
import Test.Koa.Parser.Loop
import Test.Koa.Parser.Returns
import Test.Koa.Parser.Unop
import Test.Tasty

parserTests :: [TestTree]
parserTests =
  [ testGroup "Returns" returnTests,
    testGroup "Binop" binopTests,
    testGroup "Unop" unopTests,
    testGroup "Let" letTests,
    testGroup "Loop" loopTests,
    testGroup "Assignment" assignmentTests,
    testGroup "Functions" functionsTests
  ]
