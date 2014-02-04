{-
 -
 - Copyright (c) 2011 Andrew Pennebaker
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

-- | This module describes the state of shpider computations, and provides a monad transformer over it.
module Network.Shpider.State
   ( module Control.Monad.State
   , ShpiderState (..)
   , Page (..)
   , Shpider
   , emptyPage
   , runShpider
   , runShpiderSt
   , runShpiderStWithOptions
   , runShpiderWithOptions
   , initialSt
   )
   where

import Control.Monad.State

import Network.Shpider.Curl.Curl

import Data.Maybe

import Text.HTML.TagSoup.Parsec

import Network.Shpider.Forms
import Network.Shpider.Links

-- | The shpider state holds all the options for shpider transactions, the current page and all the `CurlOption`s used when calling curl.
data ShpiderState =
   SS { htmlOnlyDownloads :: Bool
      , startPage :: String
      , dontLeaveDomain :: Bool
      , curlOpts :: [ CurlOption ]
      , currentPage :: Page
      , visited :: Maybe [ String ]
      }
   deriving Show

-- | The type of Shpider computations.  A state transformer over `ShpiderState` and `IO`.
type Shpider =
   StateT ShpiderState IO

-- | Run a Shpider computation, returning the result with the state.
runShpiderSt :: Shpider a -> IO ( a , ShpiderState )
runShpiderSt f =
   withCurlDo $ runStateT f initialSt

-- | Run a Shpider computation, returning the result.
runShpider :: Shpider a -> IO a
runShpider f = do
   ( res , _ ) <- runShpiderSt f
   return res

-- | Run a Shpider computation with otions, returning the result with the state.
runShpiderStWithOptions :: ShpiderState -> Shpider a -> IO ( a , ShpiderState )
runShpiderStWithOptions state f =
   withCurlDo $ runStateT f state

-- | Run a Shpider computation with options, returning the result.
runShpiderWithOptions :: [ CurlOption ] -> Shpider a -> IO a
runShpiderWithOptions options f = do
   ( res , _ ) <- runShpiderStWithOptions (initialSt { curlOpts = options }) f
   return res

-- | The initial shpider state.
-- Currently, CurlTimeout is hard wired to 3, and cookies are saved in a file called "cookies".
initialSt :: ShpiderState
initialSt =
   SS { startPage = ""
      , htmlOnlyDownloads = False
      , dontLeaveDomain = False
      , curlOpts = [ CurlCookieFile "cookies"
                   , CurlCookieJar "cookies"
                   ]
      , currentPage = emptyPage 
      , visited = Nothing 
      }

-- | The Page datatype.  Holds `Link`s, `Form`s, the parsed [ `Tag` ], the page source, and the page's absolute URL.
data Page =
   Page { links :: [ Link ]
        , forms :: [ Form ]
        , tags :: [ Tag String ]
        , source :: String
        , addr :: String
        }
   deriving Show

-- | An empty page, containing no information.
emptyPage :: Page
emptyPage =
   Page { links = []
        , forms = []
        , source = ""
        , tags = []
        , addr =""
        }
