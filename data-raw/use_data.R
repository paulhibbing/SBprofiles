# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

# Example data ------------------------------------------------------------

  update_example_data <- FALSE

  if (file.exists("data/example_data.rda")) {
    load("data/example_data.rda")
    if (!isTRUE(all.equal(
      readRDS("data-raw/example.rds"),
      example_data
    ))) message(
      "`example_data` has changed -- if you wish to update,",
      "\n uncomment lines 30-31",
      " (In RStudio for Windows, highlight and ctrl + shift + C)",
      "\n and run them (In RStudio for Windows, ctrl + enter)"
    ) else message(
      "example_data has not changed -- no need to update"
    )
  }

  # example_data <- readRDS("data-raw/example.rds")
  # usethis::use_data(example_data, overwrite = TRUE)

# Models ------------------------------------------------------------------

  c(
    "data-raw/tree_1min.rds",
    "data-raw/tree.rds",
    "data-raw/forest_1min.rds",
    "data-raw/forest.rds"
  ) %>%
  lapply(readRDS) %>%
  stats::setNames(c("tree1", "tree5", "forest1", "forest5")) %>%
  usethis::use_data(internal = TRUE, overwrite = TRUE)
