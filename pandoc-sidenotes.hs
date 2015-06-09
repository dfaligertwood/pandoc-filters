--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

import           Text.Pandoc.JSON
import           Utils

--------------------------------------------------------------------------------
-- This filter converts all footnotes with the relevant initial letter into
-- sidenotes using the marginnote.sty package for LaTeX.

main :: IO ()
main = toJSONFilter sidenotes

sidenotes :: Maybe Format -> Pandoc -> Pandoc
sidenotes (Just "latex")
    = unionMetaInline
        "header-includes"
        (RawInline "tex" "\\usepackage{marginnote}")
    . note ">"
           (latexInline "\\normalmarginpar\\marginnote{")
    . note "~"
           (latexInline "\\reversemarginpar\\marginnote{")
  where
    note a b = overloadNote a b (latexInline "}") id
sidenotes _ = rmStr ">"
            . rmStr "~"

--------------------------------------------------------------------------------
