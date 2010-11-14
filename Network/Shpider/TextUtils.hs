module Network.Shpider.TextUtils where

import Data.Char

import Network.URL

import Control.Arrow ( first )

-- | A case insensitive lookup for html attributes.
attrLookup :: String -> [ ( String , String ) ] -> Maybe String
attrLookup attr =
   lookup ( lowercase attr ) . map ( first lowercase )

-- | Drops whitespace from the beginning and end of strings.
trim :: String -> String
trim =
   dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Turns a String lowercase.  <rant>In my humble opinion, and considering that a few different packages implement this meager code, this should be in the prelude.</rant>
lowercase :: String -> String 
lowercase =
   map toLower

-- | Encode spaces in a URL
escapeSpaces :: String -> String
escapeSpaces =
   encString True ( \ _ -> True )

