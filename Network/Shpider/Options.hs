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

-- | This module provides all the settable options in shpider.
module Network.Shpider.Options where

import Data.Maybe

import Network.Shpider.Curl.Opts
import Network.Shpider.Curl.Types

import Network.Shpider.State
import Network.Shpider.URL
import Network.Shpider.TextUtils

-- | Setting this to `True` will forbid you to `download` and `sendForm` to any site which isn't on the domain shared by the url given in `setStartPage`.
stayOnDomain :: Bool -> Shpider ( )
stayOnDomain b = do
   shpider <- get
   put $ shpider { dontLeaveDomain = b }

-- | Set the CurlTimeout option.  Requests will TimeOut after this number of seconds.
setTimeOut :: Long -> Shpider ( )
setTimeOut s = do
   shpider <- get

   let isTimeout c =
          case c of
             (CurlTimeout _) -> True
             _ -> False

       timeoutPresent = any isTimeout $ curlOpts shpider

   put $ shpider { curlOpts =
                      if not timeoutPresent
                         then CurlTimeout s : curlOpts shpider
                         else map (\c -> if isTimeout c
                                        then CurlTimeout s
                                        else c
                                )
                                (curlOpts shpider)
                 }

-- | Set the start page of your shpidering antics.
-- The start page must be an absolute URL, if not, this will raise an error.
setStartPage :: String -> Shpider ( )
setStartPage uncleanUrl = do
   shpider <- get

   if isAbsoluteUrl url
      then put $ shpider { startPage = url }
      else error "The start page must be an absolute URL"
   where
      url = escapeSpaces uncleanUrl

-- | Return the starting URL, as set by `setStartPage`
getStartPage :: Shpider String
getStartPage = do
   shpider <- get
   return $ startPage shpider

-- | If onlyDownloadHtml is True, then during `download`, shpider will make a HEAD request to see if the content type is text\/html or application\/xhtml+xml, and only if it is, then it will make a GET request.
onlyDownloadHtml :: Bool -> Shpider ( )
onlyDownloadHtml b = do
   st <- get
   put $ st { htmlOnlyDownloads = b }

-- | Set the given page as the `currentPage`.
setCurrentPage :: Page -> Shpider ( )
setCurrentPage p = do
   shpider <- get 
   put $ shpider { currentPage = p }

-- | Return the current page
getCurrentPage :: Shpider Page
getCurrentPage = do
   sh <- get
   return $ currentPage sh

-- | When keepTrack is set, shpider will remember the pages which have been `visited`.
keepTrack :: Shpider ( )
keepTrack = do
   shpider <- get
   put $ shpider { visited = Just [] }