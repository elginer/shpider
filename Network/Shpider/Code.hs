{-
 -
 - Copyright (c) 2009-2010 Johnny Morrice
 -
 - Permission is hereby granted, free of charge, to any person
 - obtaining a copy of this software and associated documentation 
 - files (the "Software"), to deal in the Software without 
 - restriction, including without limitation the rights to use, copy, 
 - modify, merge, publish, distribute, sublicense, and/or sell copies 
 - of the Software, and to permit persons to whom the Software is 
 - furnished to do so, subject to the following conditions:
 -
 - The above copyright notice and this permission notice shall be 
 - included in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 - BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 - ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 - CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 - SOFTWARE.
 -
-}

module Network.Shpider.Code 
   ( module Network.Curl.Code
   , ccToSh
   , ShpiderCode (..)
   )
   where

import Network.Curl.Code

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
