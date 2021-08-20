rm(list = ls())
library(magrittr)

rstudioapi::getActiveDocumentContext()$path %>%
dirname(.) %>%
dirname(.) %>%
setwd(.)

source("0_Input/6a_Exclusion_Functions.R")

tree <- readRDS("2a_Cluster/rds/tree.rds")
forest <- readRDS("2a_Cluster/rds/forest.rds")

# > nrow(suppressMessages(load_and_reduce(age = 30)))
# [1] 4146

d <-
  suppressMessages(load_and_reduce(age = 30)) %>%
  .[ ,setdiff(names(.), "diabetes")] %>%
  merge(
    readRDS("0_Input/rds/vars.rds")[
      , c("id", "diabetes", "weight_status", "cvd_risk")
    ]
  ) %T>%
  {stopifnot(nrow(.) == 4146, !anyNA(.$cvd_risk))} %>% ## See line 10
  within({
    high_risk = (cvd_risk > 0.20)
    accel_valid = sapply(accel_valid, isTRUE)
  }) %>%
  .[.$accel_valid, c("id", "diabetes", "weight_status", "high_risk")]

d <-
  readRDS("1_Bout_Data/d5_final.rds") %>%
  .[ ,setdiff(names(.), "diabetes")] %>%
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
# Removing 86 PCA outliers
