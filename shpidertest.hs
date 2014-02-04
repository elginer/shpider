#!/usr/bin/env runhaskell

import Network.Shpider hiding (get)
import Network.Shpider.Curl.Opts

main = do
  (result, page) <- runShpiderWithOptions [CurlUserAgent "Windows Mozilla"] $ do
    download "http://browser.yellosoft.us/text.php"

  case result of
    Ok -> putStrLn "Ok"
    _ -> putStrLn "Error"

  putStrLn $ source page