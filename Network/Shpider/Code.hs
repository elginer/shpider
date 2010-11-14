module Network.Shpider.Code 
   ( module Network.Shpider.Curl.Code
   , ccToSh
   , ShpiderCode (..)
   )
   where

import Network.Shpider.Curl.Code

-- | Converts a `CurlCode` to a `ShpiderCode`.
ccToSh :: CurlCode -> ShpiderCode
ccToSh curlCode = 
  case curlCode of
     CurlOK ->
        Ok
     CurlHttpReturnedError ->
        HttpError
     CurlCouldntResolveHost ->
        NoHost
     CurlUnspportedProtocol ->
        UnsupportedProtocol
     CurlOperationTimeout ->
        TimeOut
     c ->
        UnsupportedCurlStatus c

-- | ShpiderCode describes the various contingencies which may occur during a shpider transaction.
data ShpiderCode =
   Ok
   | InvalidURL 
   | HttpError
   | OffSite
   | WrongData
   | NoHost
   | UnsupportedProtocol
   | TimeOut
   | UnsupportedCurlStatus CurlCode
   deriving ( Show , Eq )
