rm(list = ls())
library(magrittr)

example_data <- readRDS("data-raw/example.rds")
usethis::use_data(example_data)

# list.files("data-raw", "^[ft]", full.names = TRUE) %>% dput

c(
  "data-raw/tree_1s.rds",
  "data-raw/tree.rds",
  "data-raw/forest_1s.rds",
  "data-raw/forest.rds"
) %>%
lapply(readRDS) %>%
stats::setNames(c("tree1", "tree5", "forest1", "forest5")) %>%
usethis::use_data(internal = TRUE, overwrite = TRUE)
