# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  source("1_Bout_Data/zz_functions.R")
  source("2b_Epi/zz_functions.R")

  d1 <- readRDS("2b_Epi/0_Epi_data_1min.rds")
  d5 <- readRDS("2b_Epi/0_Epi_data.rds")

  common_ids <- intersect(d1$id, d5$id)

  d1 %<>% subset(id %in% common_ids)
  d5 %<>% subset(id %in% common_ids)

  ## Exposures

    vars <- c(
      "forest_profile", "adj_total_SB",
      "adj_sb_0_14", "adj_sb_15_29", "adj_sb_30_Inf",
      "adj_mean_SB_bout", "adj_median_sb_bout",
      "ubd_predicted", "fragmentation_index",
      "gini", "alpha"
    )

  ## Unadjusted tertiles (descriptive)

    # unadj <- c(
    #   "SB_hr_day", "sb_0_14", "sb_15_29", "sb_30_Inf", "mean_SB_bout_min",
    #   "Q50_bout", "ubd_predicted", "fragmentation_index"
    # )
    #
    # expand.grid(
    #   d = c("d1", "d5"),
    #   varname = unadj,
    #   stringsAsFactors = FALSE,
    #   KEEP.OUT.ATTRS = FALSE
    # ) %>%
    # as.list(.) %>%
    # {mapply(
    #   function(d, varname, ...) {
    #     print(paste(d, varname, sep = "; "))
    #     get(d, envir = globalenv()) %>%
    #     .[ ,varname] %>%
    #     tertilize(...) %>%
    #     levels(.) %>%
    #     .[!grepl("Inf", .)] %>%
    #     gsub("^.* ", "", .) %>%
    #     gsub("[()]", "", .) %>%
    #     list(x = .) %>%
    #     stats::setNames(
    #       ., gsub("^x$", paste0(varname, " (", d, ")"), names(.))
    #     )
    #   }, .$d, .$varname, MoreArgs = list(digits = 2, nsmall = 2),
    #   SIMPLIFY = FALSE, USE.NAMES = FALSE
    # )} %T>%
    # {cat("\n\n")}

  ## Two decimal tertiles (manual)

    # > levels(tertilize(d1$adj_median_sb_bout, 2, 2))
    # [1] "T1 (2.01)" "T2 (2.13)" "T3 (Inf)"
    # > levels(tertilize(d1$gini, 2, 2))
    # [1] "T1 (0.58)" "T2 (0.62)" "T3 (Inf)"
    # > levels(tertilize(d5$gini, 2, 2))
    # [1] "T1 (0.38)" "T2 (0.43)" "T3 (Inf)"
    # > levels(tertilize(d1$alpha, 2, 2))
    # [1] "T1 (1.90)" "T2 (2.07)" "T3 (Inf)"
    # > levels(tertilize(d5$alpha, 2, 2))
    # [1] "T1 (2.22)" "T2 (2.47)" "T3 (Inf)"

  ## Covariates

    M1 <- NULL
    M2 <- c("race", "MVPA_perc")
    M3 <- c(M2, "weartime_hr_day")

# Implementation ----------------------------------------------------------

  assemble_ORs <- function(d, vars, M1, M2, M3) {
    d %>%
    within({
      adj_total_SB = tertilize(adj_total_SB)
      adj_sb_0_14 = tertilize(adj_sb_0_14)
      adj_sb_15_29 = tertilize(adj_sb_15_29)
      adj_sb_30_Inf = tertilize(adj_sb_30_Inf)
      adj_mean_SB_bout = tertilize(adj_mean_SB_bout)
      adj_median_sb_bout = tertilize(adj_median_sb_bout)
      ubd_predicted = tertilize(ubd_predicted)
      fragmentation_index = tertilize(fragmentation_index)
      gini = tertilize(gini)
      alpha = tertilize(alpha)
    }) %>%
    {mapply(
      get_OR_table,
      vars,
      MoreArgs = list(d = ., M1 = M1, M2 = M2, M3 = M3),
      SIMPLIFY = FALSE
    )} %>%
    c(make.row.names = FALSE) %>%
    do.call(rbind, .)
  }

  list(d1, d5) %>%
  lapply(assemble_ORs, vars, M1, M2, M3) %>%
  do.call(cbind, .) %>%
  data.table::fwrite("2b_Epi/2b_Results_1minLeft_5minRight.csv")
