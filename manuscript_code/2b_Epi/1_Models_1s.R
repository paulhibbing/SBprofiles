# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  source("2b_Epi/zz_functions.R")

  d <- readRDS("2b_Epi/0_Epi_data_1s.rds")

  M1 <- NULL
  M2 <- c("race", "MVPA_perc")
  M3 <- c(M2, "daily_wear_h")

  ## This is why we should not adjust for sex (and same concept applies to age):
  # > table(d$high_risk, d$sex)
  # 
  #       Male   Female
  # FALSE  1326    1757
  # TRUE    647     230 <--- Being female is highly predictive of whether a
  #                        person can be in the high risk category, and this is
  #                        precisely because the risk calculation assumes
  #                        females have lower risk. In other words, sex has
  #                        already been accounted for when determining the
  #                        outcome variable, and it would confound the model to
  #                        then also include it as a covariate.
  
# Implementation ----------------------------------------------------------

  d %>%
  within({
    mean_SB_bout_raw = tertilize(mean_SB_bout_raw)
    mean_SB_bout_residual = tertilize(mean_SB_bout_residual)
    total_SB_residual = tertilize(total_SB_residual / n_days / 60)
    total_SB_raw = tertilize(total_SB_raw / n_days / 60)
  }) %>%
  {rbind(
    get_OR_table(., M1, M2, M3),
    get_OR_table(., M1, M2, M3, "mean_SB_bout_raw"),
    get_OR_table(., M1, M2, M3, "mean_SB_bout_residual"),
    get_OR_table(., M1, M2, M3, "total_SB_residual"),
    get_OR_table(., M1, M2, M3, "total_SB_raw")
  )} %T>%
  View("ORs") %>%
  data.table::fwrite("2b_Epi/1b_ORs_1s.csv")
