## What to expect here

This is the part of `SBprofiles` where you will find the code we used
when writing our manuscript. This is not intended to help you implement
the profiles, just to understand how they were developed. For usage help,
see the [vignette](https://github.com/paulhibbing/SBprofiles/blob/master/vignettes/SBprofiles.pdf).

**NOTE:** If you plan to replicate this code, you will need to make sure you
  have several packages installed. The following code should cover it, but
  feel free to [submit an issue](https://github.com/paulhibbing/SBprofiles/issues)
  if you run into problems.
  
```

packages <- c(
  "beepr", "caret", "cluster", "data.table", "e1071", "factoextra",
  "ggplot2", "magrittr", "PAutilities", "PhysicalActivity", "randomForest",
  "rvest", "SASxport", "tree", "xml2", "devtools"
)

lapply(
  packages,
  function(x) if (!x %in% installed.packages()) install.packages(x)
)

if (!"tree" %in% installed.packages()) {
  devtools::install_version("tree", "1.0-39") ## Older R can only install old version
}

devtools::install_github("SciViews/svDialogs")

```
