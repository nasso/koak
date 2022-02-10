import Analyser (analyserTests)
import Compiler (compilerTests)
import Parser (parserTests)
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
