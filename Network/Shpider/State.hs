-- | This module describes the state of shpider computations, and provides a monad transformer over it.
module Network.Shpider.State 
   ( module Control.Monad.State
   , ShpiderState (..)
   , Page (..)
   , Shpider
   , emptyPage
   , runShpider
   , runShpiderSt
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
        , tags :: [ Tag ]
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
