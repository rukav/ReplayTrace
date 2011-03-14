module TestBasic (running) where
import System.Time
import Log.Replay

getTime :: IO Integer
getTime = do
  TOD secs _ <- getClockTime
  return secs

example :: Replay String String Int
example = do
  t0 <- lift getTime
  lift (putStrLn "Hello!")
  age <- ask "What is your age?"
  lift (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  lift (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- lift getTime
  lift (putStrLn ("Total time: " ++ show (t1 - t0) ++ " seconds"))
  return (read age)

running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    eqa <- run prog t    -- this is the same prog every time!
    case eqa of
      Left (q,t') -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t' r)
      Right x -> return x

test1 = do x <- run example []; print x
test2 = do x <- run example [Result "1296343664",Result "()", Answer "27"]; print x
test3 = do x <- run example [Result "1296343664",Result "()", Answer "27", Result "()", Answer "Starback"]; print x