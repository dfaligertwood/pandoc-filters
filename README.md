# Pandoc Filters

Various filters I have written for use with jgm/pandoc.

Mostly already available in dfaligertwood/utility-scripts, moved here for 
clarity.


## pandoc-filter-manager

This lets you configure your filters in the YAML frontmatter of pandoc. Format 
as follows (eg):

```
---
filters:
      - pandoc-citeproc
      - pandoc-ednotes
---
```

Currently inherits PATH from whatever ran the script, so if you want it to work 
well on OS X, make sure to run it from a shell with your path configured. If 
you want it to run from marked or similar, make sure you have a wrapper script.



## pandoc-sidenotes

Uses `marginnote.sty` in LaTeX for sidenotes. Other outputs it just gives 
standard footnotes. Syntax as follows:

```
^[> sidenote appearing in the outside margin]
^[~ sidenote appearing in the inside margin]
```

## pandoc-ednotes

Uses edNotes.sty in LaTeX for editorial notes. Other outputs it gives standard 
footnotes. Add extra editorial series by editing the dict ‘power_functions’ in 
the parse function. Syntax:

```
^[| Editorial note series A]
^[& Editorial note series B]
```

## pandoc-divs

Adds the div styles `afterpage` and `center`. These only have an effect in 
LaTeX output.

