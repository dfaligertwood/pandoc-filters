--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

import           Text.Pandoc.JSON
import           Text.Pandoc.Generic
import           Data.List
  ( isInfixOf )
import           Utils

--------------------------------------------------------------------------------
-- This filter takes divs with the class "afterpage" and inserts the LaTeX
-- 'afterpage' commands. It also adds '\usepackage{afterpage}' to the header.

main :: IO ()
main = toJSONFilter afterpage

afterpage :: Maybe Format -> Pandoc -> Pandoc
afterpage (Just "latex")
    = unionMetaInline "header-includes" (RawInline "tex" "\\usepackage{afterpage}")
    . bottomUp (concatMap divToAfterpage)
afterpage _ = id

divToAfterpage :: Block -> [Block]
divToAfterpage (Div (_, cs, _) contents)
  | ["afterpage"] `isInfixOf` cs = concat
                                   [ [latexBlock "\\begin{afterpage}"]
                                   , contents
                                   , [latexBlock "\\end{afterpage}"] ]
divToAfterpage x = [x]

--------------------------------------------------------------------------------
