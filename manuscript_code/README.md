## What to expect here

This is the part of `SBprofiles` where you will find the R code I used
when writing our manuscript. This is not intended to help you implement
the profiles, just to understand how they were developed. For usage help,
please see the [PBpatterns package](https://github.com/paulhibbing/PBpatterns)
and, in particular, the associated [vignette](https://github.com/paulhibbing/PBpatterns/blob/main/vignettes/SBprofiles.pdf)
in that package.

**NOTE:** If you plan to replicate this code, you will need to make sure you
have several packages installed. The following code should cover it, but
feel free to [submit an issue](https://github.com/paulhibbing/PBpatterns/issues)
if you run into problems.
  
```

packages <- c(
  "beepr", "caret", "cluster", "data.table", "DescTools", "e1071", 
  "factoextra", "ggplot2", "magrittr", "PAutilities", "PhysicalActivity", 
  "randomForest", "remotes", "reshape2", "rstudioapi", "rvest", 
  "SASxport", "svDialogs", "tree", "xml2"
)

lapply(
  packages,
  function(x) if (!x %in% installed.packages()) install.packages(x)
)

if (!"tree" %in% installed.packages()) {
  remotes::install_version("tree", "1.0-39") ## Older R can only install old version
}

if (packageVersion("DescTools") < "0.99.32") {
    install.packages("DescTools") ## Requires a newer version of DescTools
}

## Need development versions of two packages:
remotes::install_github("paulhibbing/PAutilities", dependencies = FALSE)
remotes::install_github("SciViews/svDialogs", dependencies = FALSE)

```
