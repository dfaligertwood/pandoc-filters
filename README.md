# Pandoc Filters

Various filters I have written for use with jgm/pandoc.

Mostly already available in dfaligertwood/utility-scripts, moved here for 
clarity.

I recently rewrote them all in haskell, mostly as an educational exercise. This 
should make some of the scripts (eg. multibib) somewhat faster. To install, 
just run ‘cabal install’ in this repo after cloning it to your local machine.

NB. The format of pandoc-multibib has changed again! Sorry if you were using
it... :(


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

## pandoc-multibib

A sketch of a filter to add multiple-bibliography support to pandoc-citeproc. 
This is really the sort of functionality that should be added to 
pandoc-citeproc proper.

Current syntax looks like this:

```
---
bibliographies:
    - "source1.type": Title of Bibliography 1
    - "source2.type": Title of Bibliography 2
bibliography: complete.type
---
```

Note that there HAS TO BE a complete bibliography. This also permits 
compilation when pandoc-multibib is not available.

Sample of the tool in use (excluding test bibliographies):

```
---
filters:
  - pandoc-multibib
bibliographies:
  - test1.json: First Bibliography
  - test2.json: Second Bibliography
bibliography: test.json
---

[@Shepherd_1961_i3] [@Square_1884_i5]
```

and the result:

```
(Shepherd 1961) (Square 1884)



FIRST BIBLIOGRAPHY


Shepherd, Massey H., Jr. 1961. “The Formation and Influence of the
Antiochene Liturgy.” _Dumbarton Oaks Papers_ 15 (January): 23–44.
doi:10.2307/1291174. http://www.jstor.org/stable/1291174.



SECOND BIBLIOGRAPHY


Square, A. 1884. _Flatland: A Romance of Many Dimensions_. London:
Seeley & Co.


```

## pandoc-cv

cv generation helper.
