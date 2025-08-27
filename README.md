This is a repository of some simple code that I find useful, but is not worth making into a package. 

* `bigPalette.R` defines a vector of distinct colors useful for coloring categorical variables (over 50 distinct values)
* `massivePalette.R` defines a vector of hundreds of colors, starting with those of `bigPalette` and then completed with randomly ordering the colors `colors()` (removing the greyscale colors.) This is mainly useful to make sure you won't hit errors with not having the length of the color vector large enough.
* `showPalette.R` defines a function that will plot small representations of each color in a vector of color, with the index number of the color next to it. This is useful for identifying which colors to extract from a vector of colors.
* `setBreaks.R` defines a function for creating a vector of breaks for a data set. The resulting breaks are intended to be given to a heatmap function. The function has common needed adjustments, for example
  - making the breaks symmetric around 0
  - making all points greater than a user-defined % of the data the same color to reduce effect of outliers.

This code can be sourced directly into R with the url of the file, which can be found by clicking on the file within github, and then clicking on the button labeled "Raw". 

For example,

```
rcodeURL<-"https://raw.githubusercontent.com/epurdom/purdom-public-code/refs/heads/main/"
source(file.path(rcodeURL,"bigPalette.R"))
```