import System.Time
import Replay
import TestBasic (running)

askAge :: Replay String String Int
askAge = cut $ do
  birth <- ask "What is your birth year?"
  now <- ask "What is the current year?"
  return (read now - read birth)

ret :: Replay String String Int
ret = cut $ do return 0

nest :: Replay String String Int
nest = do
   mage <- askAge
   yage <- askAge
   return (yage - mage)

nestCut :: Replay String String Int
nestCut = cut $ do
   mage <- askAge
   yage <- askAge
   return (yage - mage)

test  = running askAge
test1 = running ret
test2 = running nest

test31 = do x <- run nest []; print x
test32 = do x <- run nest [Cut,Answer "27"]; print x
test33 = do x <- run nest [Cut,Answer "27",Answer "30"]; print x
test34 = do x <- run nest [Summary "3", Cut, Answer "100"]; print x
test35 = do x <- run nest [Summary "3", Cut, Answer "100", Answer "150"]; print x

test41 = do x <- run nestCut []; print x
test42 = do x <- run nestCut [Cut, Cut, Answer "27"]; print x
test43 = do x <- run nestCut [Cut, Cut, Answer "27",Answer "30"]; print x
test44 = do x <- run nestCut [Cut, Summary "3", Cut, Answer "100"]; print x
test45 = do x <- run nestCut [Cut, Summary "3", Cut, Answer "100", Answer "150"]; print x
