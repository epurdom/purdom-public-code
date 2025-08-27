This is a repository of some simple code that I find useful, but is not worth making into a package. 

* `bigPalette.R` defines a vector of distinct colors useful for coloring categorical variables (58 distinct values)
* `massivePalette.R` defines a vector of hundreds of colors, starting with those of `bigPalette` and then completed with randomly ordering the colors `colors()` (removing the greyscale colors.) This is mainly useful to make sure you won't hit errors with not having the length of the color vector large enough.
* `showPalette.R` defines a function that will plot small representations of each color in a vector of color, with the index number of the color next to it. This is useful for identifying which colors to extract from a vector of colors.

This code can be sourced directly into R with the url of the file. For example,

```
rcodeURL<-"https://github.com/epurdom/purdom-public-code/blob/main"
source(file.path(rcodeURL,"bigPalette.R"))
```