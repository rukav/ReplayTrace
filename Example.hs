import Cgi
import Text.XHtml

formContent = [paragraph << ("Type something here " +++ textfield "input_example"),
                       submit "" "Submit"]
page s = header << body << paragraph << ("Hello " ++ s ++ "!")

cgi :: Cgi ()
cgi = do 
   vars <- ask formContent
   let s = getInput "input_example" vars
   lift $ putStr $ defaultContent ++ renderHtml (page s)

main = runCGI cgi