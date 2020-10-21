rm(list = ls())
library(magrittr)

d <- readRDS("2a_Cluster/rds/clustered_d.rds")

source("2a_Cluster/zz_features.R")

descriptives <- function(d, label, variables) {
  
  d[ ,variables] %>%
  sapply(function(x) {
    ifelse(mean(x) < 1, 3, 1) %>%
    PAutilities::mean_sd(
      x, give_df = FALSE, digits = ., nsmall = .
    )
  }, simplify = FALSE) %>%
  do.call(rbind, .) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  stats::setNames(label)
  
}

split(d, d$cluster) %>%
c(list(total = d)) %>%
{mapply(
  descriptives, ., names(.),
  MoreArgs = list(variables = variables),
  SIMPLIFY = FALSE
)} %>%
do.call(data.frame, .) %>%
data.table::fwrite(
  "2a_Cluster/2_Descriptives.csv", row.names = TRUE
)
