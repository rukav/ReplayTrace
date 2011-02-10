module Replay 
  (
    Replay
  , run
  , lift
  , ask
  , cut
  , Trace
  , emptyTrace
  , addAnswer
  , Item (..)
  ) 
where

import System.Time

type Trace r = [Item r]
type Counter = Int

data Item r
  = Answer r
  | Result String
  | Cut
  | Summary String
 deriving ( Show, Read )

emptyTrace :: Trace r
emptyTrace = []

addAnswer :: Trace r -> r -> Trace r
addAnswer t a = t ++ [Answer a]

type RepState r = (Trace r, Counter)
newtype Replay q r a = R { unR :: RepState r -> IO (RepState r, Either q a) }

instance Monad (Replay q r) where
  return x = R $ \t -> return $ (t, Right x)
  R x >>= k = R $ \t -> do
       (t',r) <- x t
       case r of
        Left q -> return $ (t', Left q)
        Right v -> unR (k v) t' 

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (R f) t = do 
      (s,r) <- f (t,0)
      case r of
        Left q -> return $ Left (q,fst s)
        Right v -> return $ Right v


lift :: (Show a, Read a) => IO a -> Replay q r a
lift io = 
    R $ \(t,i) -> do
          case getItem t i of
            Nothing -> do x <- io
                          return ((t ++ [Result (show x)], i+1), Right x)
            Just item -> case item of
                           Result r -> return ((t,i+1), Right $ read r)
                           _ -> expect "Result" i

ask :: q -> Replay q r r
ask q = 
    R $ \(t,i) -> do
          case getItem t i of
            Nothing -> return ((t,i), Left q)
            Just item -> case item of
                           Answer r -> return ((t,i+1), Right r)
                           _ -> expect "Answer" i

cut :: (Read a, Show a) => Replay q r a -> Replay q r a
cut (R x) = 
    R $ \(t,i) -> do
          case getItem t i of
            Nothing -> x (t ++ [Cut],i+1) >>= mkCut 
            Just item -> 
                case item of
                  Cut -> x (t,i+1) >>= mkCut 
                  Summary s -> return ((t,i+1), Right $ read s)
                  _ -> expect "Cut or Summary" i
   where mkCut res@((t,i),v) =
            case v of 
              Left _ -> return res
              Right r -> let (t',i') = cutTrace t in
                         return ((t' ++ [Summary (show r)], i'+1), Right r)
         cutTrace [] = error "Missing Cut item"
         cutTrace xs = 
             (take cutInd xs, cutInd) where
                   cutInd = snd $ last $ filter isCut $ zip xs [0..]
                   isCut (Cut,_) = True
                   isCut (_,_) = False        

getItem :: Trace r -> Int -> Maybe (Item r)
getItem t i = let top = drop i t in
   if null top then Nothing else Just $ head top

expect item i = error err where
  err = "Expect " ++ item ++ " item in the trace (index=" ++ show i ++ ")"
