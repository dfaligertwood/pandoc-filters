--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
--------------------------------------------------------------------------------

import           Text.Pandoc.JSON
import           Text.Pandoc.Generic
import           Text.Pandoc.Walk
import           Data.List
  ( isInfixOf )
import           Data.List.Split
  ( splitOn )
import           Utils

--------------------------------------------------------------------------------
-- This filter takes divs with the class "edition" and inserts the LaTeX
-- commands for an ednotes environment. It then converts all footnotes with
-- the relevant initial letters into ednotes.sty editorial comments.

rmKeys :: Walkable Inline a => a -> a
rmKeys = walk $ rmStr "|" . rmStr "&"

main :: IO ()
main = toJSONFilter ednotes

ednotes :: Maybe Format -> Pandoc -> Pandoc
ednotes (Just "latex")
    = unionMetaInline "header-includes"
                      (RawInline "tex" "\\usepackage[Apara, Bpara]{ednotes}")
    . topDown (concatMap processDivs)
ednotes _ = bottomUp (concatMap $ mvNotes "|")
          . bottomUp (concatMap $ mvNotes "&")

processDivs :: Block -> [Block]
processDivs (Div (_, cs, _) contents)
  | ["edition"] `isInfixOf` cs
        = concat
          [ [beginText]
          , addNotes contents
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
    addNotes
      = overloadNote "|" (latexInline "\\Anote{") (latexInline "}") makeSplit
      . overloadNote "&" (latexInline "\\Bnote{") (latexInline "}") makeSplit
processDivs x = [rmKeys x]

makeSplit :: [Inline] -> [Inline]
makeSplit
    = concat
    . uncurry ((++) . (++ [[latexInline "}{"]]))
    . splitAt 1
    . splitOn [Space, Str ":", Space]

mvNotes :: String -> Inline -> [Inline]
mvNotes char n@(Note (Para (c:_) : _))
  | c == Str char = mvNote $ rmStr char n
  where
    mvNote (Note (Para cs : ps))
        = (\ (w : w') -> w ++ [Note $ Para (concat w') : ps])
        $ splitOn [Space, Str ":", Space] cs
mvNotes _ x = [x]

--------------------------------------------------------------------------------
