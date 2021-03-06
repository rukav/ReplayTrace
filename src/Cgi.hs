-- | An example of CGI library based on the Replay monad.
-- The idea is that the CGI programmer can send HTML forms to the user
-- by using ask. When the user enters data in the form and sends it
-- back, the CGI program is re-run up to the point where ask was used,
-- and continues with the answer from the user.
module Cgi (
    module Log.Replay,
    Cgi (..),
    runCGI,
    getInput,
    defaultContent
) where
import Log.Replay
import Data.Maybe
import Network.CGI.Protocol
import Text.XHtml

type Cgi a = Replay Question Answer a

type Question = [Html]
type Answer = [(String, String)]

runCGI :: Cgi () -> IO ()
runCGI prog = do
     cont <- getContents
     if null cont then play emptyTrace
      else let vars = formDecode cont
               t = getInput traceVar vars
               trace = read t :: Trace Answer in
             play (addAnswer trace vars)
  where
  play t = do
    eqa <- run prog t 
    case eqa of
      Left (q,t') -> do
        let page = renderHtml $ (form << addTrace q t') ! [method "POST"]
        putStr $ defaultContent ++ page
      Right x -> return ()

traceVar = "TRACE"
defaultContent = unlines $ [ "Content-type: text/html", ""]

addTrace :: (Show r) => Question -> Trace r -> Question
addTrace q t = hidden traceVar (show t) : q

getInput :: String -> [(String,String)] -> String
getInput name = fromMaybe "" . lookup name

