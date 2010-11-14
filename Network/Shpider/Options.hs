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
   put $ shpider { dontLeaveDomain =
                      b
                 }

-- | Set the CurlTimeout option.  Requests will TimeOut after this number of seconds.
setTimeOut :: Long -> Shpider ( )
setTimeOut s = do
   shpider <- get
   let isTimeout c =
          case c of
             ( CurlTimeout _ ) ->
                True
             _ ->
                False
       timeoutPresent =
          not $ null $ filter isTimeout $ curlOpts shpider
   put $ shpider { curlOpts =
                      if not timeoutPresent
                         then
                            CurlTimeout s : curlOpts shpider
                         else
                            map ( \ c ->
                                     if isTimeout c
                                        then
                                           CurlTimeout s
                                        else
                                           c
                                )
                                ( curlOpts shpider )
                 }  

-- | Set the start page of your shpidering antics.
-- The start page must be an absolute URL, if not, this will raise an error.
setStartPage :: String -> Shpider ( )
setStartPage uncleanUrl = do
   shpider <- get
   if isAbsoluteUrl url
      then         
         put $ shpider { startPage =
                            url 
                       }
      else
         error "The start page must be an absolute URL"
   where
   url =
      escapeSpaces uncleanUrl

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
   put $ shpider { visited = Just [ ] }
