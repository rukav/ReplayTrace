import Data.IORef
import Replay

import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Gen (Gen (..))
import qualified Test.QuickCheck.Monadic as M
import System.Random

import Control.Monad
import Data.List (partition)

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) emptyTrace inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO ()
checkTestCase (TestCase name i r p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i
  if r == r'
    then putStrLn "ok"
    else putStrLn $ "FAIL: expected " ++ show r ++
                    " instead of " ++ show r'

-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        lift tick
        a <- ask () -- should be 3
        b <- lift (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    },
    TestCase
    { testName    = "test2"
    , testInput   = []
    , testResult  = (0,0)
    , testProgram = \tick -> do
        return 0
    },
    TestCase
    { testName    = "test3"
    , testInput   = []
    , testResult  = (0,1)
    , testProgram = \tick -> do
        lift tick
        return 0
    },
    TestCase
    { testName    = "test4"
    , testInput   = [1]
    , testResult  = (1,0)
    , testProgram = \tick -> do
        a <- ask ()
        return a
    },
    TestCase
    { testName    = "test5"
    , testInput   = [1..]
    , testResult  = (6,0)
    , testProgram = \tick -> do
        a <- ask ()
        b <- ask ()
        c <- ask ()
        return (a + b + c) 
    },
    TestCase
    { testName    = "test6"
    , testInput   = [1..]
    , testResult  = (6,1)
    , testProgram = \tick -> do
        rs <- sequence $ 
           map (\x -> if x == 1 then ask () 
                       else lift (tick >> return 0)
               ) [0,1,1,1]
        return (sum rs) 
    }
  ] 

-- | Running all the test cases.
runTests = mapM_ checkTestCase testCases

runTests' = do
   qt <- sample' $ vectorOf 50 (arbitrary :: Gen TestCase)
   let tests = map mkName (zip (head qt) [1..])
   mapM_ checkTestCase tests
   where mkName (tc@(TestCase n _ _ _), i) = 
                tc {testName = n ++ "_" ++ show i}

runTestsQC = quickCheck prop_testCase

instance Arbitrary TestCase where
   arbitrary = do
      let testName = "testQC"
      fs <- listOf (choose (0,1) :: Gen Int)
      let genActions tick = 
           map (\x->if x == 1 then ask () else lift (tick >> return 0)) fs
      let (asks, ticks) = partition (==1) fs
      testInput <- vectorOf (length asks) (choose (1,100) :: Gen Int)
      let testResult = (sum testInput, length ticks)
      let f = \tick -> do {rs <- sequence (genActions tick); return (sum rs) }
      return $ TestCase testName testInput testResult f

prop_testCase :: Property
prop_testCase = M.monadicIO $ do
    (TestCase name i r p) <- M.pick (arbitrary :: Gen TestCase)
    r' <- M.run $ runProgram p i
    M.assert $ r == r'

instance Show TestCase where
  show (TestCase name i r p) = "TestCase {" ++ 
         "testName = " ++ show name ++
         "testInput = " ++ show i ++
         "testResult = " ++ show r ++
         "testProgram = <Function> }"


