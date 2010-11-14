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
