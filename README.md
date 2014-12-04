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

## pandoc-multibib

A sketch of a filter to add multiple-bibliography support to pandoc-citeproc. 
This is really the sort of functionality that should be added to 
pandoc-citeproc proper.

Unfortunately, this doesn’t *currently* work very well: it will make 
bibliographies work, but it will only give in-text citations for the final 
bibliography file which passed over the text.

Current syntax looks like this:

```
---
bibliographies:
    - ['bibliography1.whatever', 'bibliography title', 'title-id']
    - ['bibliography2.whatever', 'bibliography title', 'title-id']
---
```

to explain the current problems, here is a sample (not including bibliographic 
files):

```
---
bibliographies:
	- ['test1.json', 'Primary Sources', 'PrimarySources']
	- ['test2.json', 'Secondary Sources', 'SecondarySources']
---

[@Test1, p. 10]

[@Test2, p. 15]
```

and the result:

```
(???)

(Bartlett and Bruce 2014, 15)



PRIMARY SOURCES


Fiske, Roger. 1986. _English Theatre Music in the Eighteenth Century_.
2nd ed. Oxford: Oxford University Press.



SECONDARY SOURCES


Bartlett, Ian, and Robert J. Bruce. 2014. “Boyce, William.” In _Grove
Music Online_. Oxford Music Online. Oxford University Press. Accessed
July 30.
http://www.oxfordmusiconline.com/subscriber/article/grove/music/40029.

```

or, in short: mostly unusable as it stands. May be updated in the future, or be 
used as a basis by someone else.
