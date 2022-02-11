import Test.Analyser (analyserTests)
import Test.Compiler (compilerTests)
import Test.Parser (parserTests)
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
