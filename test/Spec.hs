import Test.Koa.Analyser (analyserTests)
import Test.Koa.Compiler (compilerTests)
import Test.Koa.Parser (parserTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ testGroup "Parser" parserTests,
        testGroup "Analyser" analyserTests,
        testGroup "Compiler" compilerTests
      ]
