--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

import           Text.Pandoc.JSON
import           Text.Pandoc.Generic
import           Data.List
  ( isInfixOf )
import qualified Data.Map as M
import           Data.Map
  ( Map )
import           Utils

--------------------------------------------------------------------------------
-- This filter takes divs with the class "edition" and inserts the LaTeX
-- commands for an ednotes environment. It then converts all footnotes with
-- the relevant initial letters (see edNoteKeys) into ednotes.sty editorial
-- comments.

ednoteKeys :: Map String Inline
ednoteKeys = M.fromList
              [ ("|", latexInline "\\Anote{")
              , ("&", latexInline "\\Bnote{")
              ]

main :: IO ()
main = toJSONFilter ednotes

ednotes :: Maybe Format -> Pandoc -> Pandoc
ednotes (Just "latex")
    = unionMetaInline
        "header-includes"
        (RawInline "tex" "\\usepackage[Apara, Bpara]{ednotes}")
    . bottomUp (concatMap addEdnotes)
ednotes _ = id

addEdnotes :: Block -> [Block]
addEdnotes (Div (_, cs, _) contents)
  | ["edition"] `isInfixOf` cs 
        = concat
          [ [beginText]
          , bottomUp (concatMap footnotesToEdnotes) contents
          , [endText]
          ]
  where
    beginText
      = latexBlock
      $ unlines
        [ "\\begin{verse}"
        , "\\begin{linenumbers}"
        , "\\modulolinenumbers[2]"
        , "\\firstlinenumber{1}"
        ]
    endText
      = latexBlock
      $ unlines
        [ "\\end{linenumbers}"
        , "\\end{verse}"
        ]
addEdnotes x = [x]

footnotesToEdnotes :: Inline -> [Inline]
footnotesToEdnotes (Note contents)
  = if M.member firstLetter ednoteKeys
      then concat
           [ [ ednoteKeys M.! firstLetter ]
           , concatMap toInline contents
           , [ latexInline "}" ]
           ]
      else [Note contents]
  where
    firstLetter = take 1 $ show contents
footnotesToEdnotes x = [x]

--------------------------------------------------------------------------------
