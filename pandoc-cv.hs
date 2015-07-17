--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------

import          Text.Pandoc.JSON
import          Text.Pandoc
  ( writeLaTeX )
import          Text.Pandoc.Options
  ( def )
import          Utils
import          Data.List
  ( intercalate 
  , elem )

--------------------------------------------------------------------------------

main :: IO ()
main = toJSONFilter cvList

cvList :: Maybe Format -> Block -> [Block]
cvList (Just "latex") (Div (_, cs, _) contents)
  | "aside" `elem` cs
    =  [ latexBlock "\\begin{aside}" ]
    ++ contents
    ++ [ latexBlock $ unlines
                    [ "\\end{aside}"
                    , "\\hspace{0.5cm}"
                    , "\\begin{minipage}[t]{\\linewidth-0.5\\asidewidth}"
                    , "\\minipageopentrue"
                    , "\\vspace*{0.9\\baselineskip}"
                    ]
       ]
cvList (Just "latex") (DefinitionList contents)
    =  [latexBlock "\\begin{entrylist}"]
    ++ map toEntry contents
    ++ [latexBlock "\\end{entrylist}"]
cvList (Just "latex") (RawBlock "html" "<!-- break -->")
    = return
    $ latexBlock $ unlines
                 [ "\\ifminipageopen"
                 , "\\end{minipage}"
                 , "\\fi"
                 , "\\minipageopenfalse"
                 ]
cvList _ x = [x]

toEntry :: ([Inline], [[Block]]) -> Block
toEntry (date, definitions)
    = latexBlock
    $ "\\entry{"
      ++ intercalate
          "}{"
          (addBreaks (map (writeLaTeX def . Pandoc nullMeta)
                     (take 4 $ [Plain date] : definitions ++ repeat [Null]) ))
      ++ "}"
  where
    addBreaks
      = zipWith (\ i e -> if i == 3 && not (null e) then "\\\\" ++ e else e)
                [0..]
--------------------------------------------------------------------------------
