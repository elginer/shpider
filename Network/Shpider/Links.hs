module Network.Shpider.Links 
   ( Link (..)
   , gatherLinks
   , allLinks
   )
   where

import Data.Maybe

import Text.HTML.TagSoup.Parsec
import Network.Shpider.TextUtils

-- | Parse all links from a list of tags.
gatherLinks :: [ Tag String ] -> [ Link ]
gatherLinks =
   tParse allLinks

-- | The parser responsible for getting all the links.
allLinks :: TagParser String [ Link ]
allLinks = do
   ls <- allWholeTags "a"
   return $ toLinks ls

toLinks tags =
   catMaybes $ map toLink tags

toLink ( TagOpen _ attrs , innerTags , _ ) = do
   address <- attrLookup "href" attrs
   return $ Link { linkAddress = address
                 , linkText = innerText innerTags
                 }
-- | Links have an address, corresponding to the href attribute, and some inner tex.
data Link =
   Link { linkAddress :: String
        , linkText :: String
        }
   deriving ( Show , Eq )
