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
-- the relevant initial letters (see ednoteKeys) into ednotes.sty editorial
-- comments.

ednoteKeys :: [(String, Inline)]
ednoteKeys = [ ("|", latexInline "\\Anote{")
             , ("&", latexInline "\\Bnote{")
             ]

rmKeys :: Walkable Inline a => a -> a
rmKeys = walk $ foldr1 (.) $ (rmStr . fst) <$> ednoteKeys

main :: IO ()
main = toJSONFilter ednotes

ednotes :: Maybe Format -> Pandoc -> Pandoc
ednotes (Just "latex")
    = unionMetaInline
        "header-includes"
        (RawInline "tex" "\\usepackage[Apara, Bpara]{ednotes}")
    . bottomUp (concatMap processDivs)
ednotes _ = rmKeys

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
      = foldr1 (.)
      $ ($ makeSplit)
      <$> ($ latexInline "}")
      <$> (uncurry overloadNote)
      <$> ednoteKeys

processDivs x = [rmKeys x]

makeSplit :: [Inline] -> [Inline]
makeSplit
    = concat -- -> [Inline]
    . uncurry ((++) . (++ [[latexInline "}{"]])) -- -> [[Inline]]
    . splitAt 1         -- -> ([[Inline]], [[Inline]])
    . splitOn [Space, Str ":", Space] -- -> [[Inline]]

--------------------------------------------------------------------------------
