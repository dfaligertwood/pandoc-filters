--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

import           Text.Pandoc.JSON
import           Text.Pandoc.Generic
import qualified Data.Map as M
import           Data.Map
  ( Map )
import           Utils

--------------------------------------------------------------------------------
-- This filter converts all footnotes with the relevant initial letter into
-- sidenotes using the marginnote.sty package for LaTeX.

sidenoteKeys :: Map String Inline
sidenoteKeys = M.fromList
              [ (">", latexInline "\\normalmarginpar\\marginnote{")
              , ("~", latexInline "\\reversemarginpar\\marginnote{")
              ]

main :: IO ()
main = toJSONFilter sidenotes

sidenotes :: Maybe Format -> Pandoc -> Pandoc
sidenotes (Just "latex")
    = unionMetaInline
        "header-includes"
        (RawInline "tex" "\\usepackage{marginnote}")
    . bottomUp (concatMap addSidenotes)
sidenotes _ = id

addSidenotes :: Inline -> [Inline]
addSidenotes (Note contents)
  = if M.member firstLetter sidenoteKeys
      then concat
           [ [ sidenoteKeys M.! firstLetter ]
           , concatMap toInline contents
           , [ latexInline "}" ]
           ]
      else [Note contents]
  where
    firstLetter = take 1 $ show contents
addSidenotes x = [x]

--------------------------------------------------------------------------------
