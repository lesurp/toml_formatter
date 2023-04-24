import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Parser
import ParserBase
import Combinators

simpleAssignment = Assignment "pouf" "3.0" (Just (C "Wow maaan"))

testSpace = assertEqual "testSpace" EmptyLine (head $ parseToml "   \n")
testComment = assertEqual "testComment" (Comment (C "pifpafpouf")) (head $ parseToml " #pifpafpouf \n")
testAssign = assertEqual "testAssign" (Assignment "pouf" "3.0" Nothing) (head $ parseToml " pouf = 3.0 \n")
testAssign2 = assertEqual "testAssign2" simpleAssignment (head $ parseToml " pouf = 3.0  #   Wow maaan\n")

testTable = assertEqual "testTable" (Table "pouf" Nothing []) (head $ parseToml " [pouf]\n")
testTable2 = assertEqual "testTable" (Table "pouf" Nothing [simpleAssignment]) (head $ parseToml " [pouf]\n  pouf =   3.0  #   Wow maaan\n")
testTable3 = assertEqual "testTable" (Table "pouf" (Just (C "pifpafpouf")) []) (head $ parseToml " [pouf] ## pifpafpouf\n")

testTakeWord = assertEqual "testTakeWord" (" =", Right "pouf") (t "pouf =") where (P t) = takeWord

main :: IO ()
main = defaultMainWithOpts
       [
           testCase "testSpace" testSpace,
           testCase "testComment" testComment,
           testCase "testAssign" testAssign,
           testCase "testAssign2" testAssign2,
           testCase "testTable" testTable,
           testCase "testTable2" testTable2,
           testCase "testTable3" testTable3,

           testCase "testTakeWord" testTakeWord
       ]
       mempty
