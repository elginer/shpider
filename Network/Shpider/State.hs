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

-- | This module describes the state of shpider computations, and provides a monad transformer over it.
module Network.Shpider.State 
   ( module Control.Monad.State.Strict
   , ShpiderState (..)
   , Page (..)
   , Shpider
   , emptyPage
   , runShpider
   , runShpiderSave
   , runShpiderSt
   , getPageFile
   )
   where

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import qualified Data.Map                   as M
import           Data.Map                   (Map, (!))
import           Data.Maybe
import           Data.Time
import           Data.Word
import           Network.Curl
import           Network.Shpider.Forms
import           Network.Shpider.Links
import           System.Directory
import           System.FilePath.Posix
import           Text.HTML.TagSoup.Parsec


-- | The shpider state holds all the options for shpider transactions, the current page and all the `CurlOption`s used when calling curl.
data ShpiderState =
   SS { htmlOnlyDownloads :: Bool
      , startPage :: String
      , dontLeaveDomain :: Bool
      , curlOpts :: [ CurlOption ]
      , currentPage :: Page 
      , visited :: Maybe [ String ]
      , downloadThrottle :: Maybe Int
      -- ^ Whether to wait at least N micro-seconds between downloads
      -- or form submissions. Defaults to 'Nothing'.
      , lastDownloadTime :: !(Maybe UTCTime)

      , pageFilenames :: Map String String
      , offlineMode :: Bool
      , pageSaveDir :: Maybe FilePath
      , pageCount :: !Word64
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
runShpider k = evalShpiderWith k initialSt

-- | Run a Shpider computation and using the specified directory to save
-- (or read if they exist) the pages from disk.
runShpiderSave :: Bool -> FilePath -> Shpider a -> IO a
runShpiderSave offline pageDir k = do
    exists <- doesDirectoryExist pageDir
    unless exists $ createDirectory pageDir
    evalShpiderWith k $
        initialSt { offlineMode = offline
                  , pageSaveDir = Just pageDir
                  }

evalShpiderWith :: Shpider a -> ShpiderState -> IO a
evalShpiderWith k s = withCurlDo $ evalStateT k s

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
      , downloadThrottle = Nothing
      , lastDownloadTime = Nothing
      , pageFilenames = M.empty
      , offlineMode = False
      , pageSaveDir = Nothing
      , pageCount = 0
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


------------------------------------------------------------------------------
-- | Gets and increments the next page count.
nextPageCount = do
    next <- gets pageCount
    modify (\s -> s { pageCount = pageCount s + 1 })
    return $ show next


------------------------------------------------------------------------------
-- | Gets the filename for a stored page.
getPageFile :: String -> Shpider (Maybe FilePath)
getPageFile url = runMaybeT $ do
    dir <- MaybeT $ gets pageSaveDir
    mname <- lift $ gets (M.lookup url . pageFilenames)
    let pad len s = replicate (len - length s) '0' ++ s
        complete s = dir </> pad 3 s ++ ".html"
    lift $ complete `fmap` maybe nextPageCount return mname


