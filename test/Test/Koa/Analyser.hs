module Test.Koa.Analyser (analyserTests) where

import Test.Koa.Analyser.ControlFlow
import Test.Koa.Analyser.Functions
import Test.Koa.Analyser.Operators
import Test.Tasty

analyserTests :: [TestTree]
analyserTests =
  [ testGroup "Control flow" controlFlowTests,
    testGroup "Functions" functionsTests,
    testGroup "Operators" operatorsTests
  ]
