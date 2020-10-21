rm(list = ls())
library(magrittr)

tree <- readRDS("2a_Cluster/rds/tree.rds")
forest <- readRDS("2a_Cluster/rds/forest.rds")

d <-
  readRDS("0_Input/rds/DVs.rds") %>%
  within({
    high_risk = (cvd_risk > 0.20)
    accel_valid = sapply(accel_valid, isTRUE)
  }) %>%
  .[.$accel_valid, c("id", "high_risk")]

d <-
  readRDS("1_Bout_Data/d5_final.rds") %>%
  merge(d) %>%
  data.frame(
    .,
    tree_profile = tree:::predict.tree(tree, ., "class"),
    forest_profile = randomForest:::predict.randomForest(forest, .)
  ) %T>%
  {rm(tree, forest, envir = globalenv())}

readRDS("2a_Cluster/rds/clustered_d.rds") %>%
{setdiff(d$id, .$id)} %>%
{d$id %in% .} %T>%
{warning("Removing ", sum(.), " PCA outliers", call. = FALSE)} %>%
{d[!., ]} %>%
saveRDS("2b_Epi/0_Epi_data.rds")

# Warning message:
# Removing 44 PCA outliers 
