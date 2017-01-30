import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.LSeq (LSeq, fromList, toList, insert, remove, empty, size, alloc)
import System.Random

main :: IO ()
main = defaultMainWithOpts
        [ testCase "empty" testEmpty
        , testCase "fromList" testFromList
        , testCase "alloc" testAlloc ]
        mempty

rgen :: StdGen
rgen = mkStdGen 0

emptySeq :: LSeq a
emptySeq = empty rgen

testEmpty :: Assertion
testEmpty = assertEqual "" 0 (size emptySeq)

testFromList :: Assertion
testFromList = assertEqual "" ["a","b","c"] (toList (fromList ["a","b","c"] rgen))

testAlloc :: Assertion
testAlloc = assertEqual "" ([], []) (id, s)
  where
    ((id, g), s) = alloc [0] [100] emptySeq
