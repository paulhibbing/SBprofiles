# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  source("2b_Epi/zz_functions.R")

  ## 1-s variables need to be handled here (rather than `1_Models_1min.R`)
  ## because the 1-s epi dataset was based on different PCA outliers
  ## and thus would not allow a direct comparison.

    one_sec_vars <- c(
      "mean_SB_bout_min", "adj_mean_SB_bout",
      "SB_hr_day", "adj_total_SB"
    )

    d <-
      readRDS("1_Bout_Data/d1_final.rds") %>%
      .[ ,c("id", one_sec_vars)]

    d <-
      readRDS("2b_Epi/0_Epi_data.rds") %>%
      .[ ,!names(.) %in% one_sec_vars] %>%
      merge(d)

  ## Covariates

    M1 <- NULL
    M2 <- c("race", "MVPA_perc")
    M3 <- c(M2, "weartime_hr_day")

  ## This is why we should not adjust for sex (and same concept applies to age):
  # > table(d$high_risk, d$sex)
  #
  #        Male  Female
  # FALSE  1365    1778
  # TRUE    670     247 <--- Being female is highly predictive of whether a
  #                        person can be in the low risk category, and this is
  #                        precisely because the risk calculation assumes
  #                        females have lower risk. In other words, sex has
  #                        already been accounted for when determining the
  #                        outcome variable, and it would confound the model to
  #                        then also include it as a covariate.

# Implementation ----------------------------------------------------------

  d %>%
  within({
    mean_SB_bout_min = tertilize(mean_SB_bout_min)
    adj_mean_SB_bout = tertilize(adj_mean_SB_bout)
    adj_total_SB = tertilize(adj_total_SB)
    SB_hr_day = tertilize(SB_hr_day)
  }) %>%
  {rbind(
    get_OR_table(., M1, M2, M3),
    get_OR_table(., M1, M2, M3, "mean_SB_bout_min"),
    get_OR_table(., M1, M2, M3, "adj_mean_SB_bout"),
    get_OR_table(., M1, M2, M3, "adj_total_SB"),
    get_OR_table(., M1, M2, M3, "SB_hr_day")
  )} %T>%
  View("ORs") %>%
  data.table::fwrite("2b_Epi/1b_ORs.csv")
