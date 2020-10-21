# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  
  clustering <- readRDS("2a_Cluster/rds/clustered_d.rds")
  cvd <- readRDS("2b_Epi/0_Epi_data.rds")

# Functions ---------------------------------------------------------------

  get_characteristics <- function(x, ...) {
    UseMethod("get_characteristics", x)
  }

  get_characteristics.factor <- function(x, label, ...) {
    table(x, useNA = "always") %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(c("varname", "n_perc")) %>%
    within({
      varname = paste0("  ", varname)
      n_perc = paste0(
        n_perc, " (", round(n_perc/sum(n_perc)*100, 1), "%)"
      )
    }) %>%
    rbind(data.frame(varname = label, n_perc = NA), .)
  }
  
  get_characteristics.numeric <- function(x, label, ...) {
    ifelse(mean(x, na.rm = TRUE) < 1, 3, 1) %>%
    PAutilities::mean_sd(
      x, give_df = FALSE, digits = ., nsmall = .
    ) %>%
    paste0(" (", sum(is.na(x)), " missing)") %>%
    data.frame(
      varname = label, n_perc = ., stringsAsFactors = FALSE
    )
  }
  
  get_characteristics.logical <- function(x, label, ...) {
    factor(x, c("FALSE", "TRUE")) %>%
    get_characteristics(label)
  }
  
  get_characteristics.default <- function(x, label, ...) {
    NULL
  }
  
  wrapper <- function(d) {
    as.list(d) %>%
    mapply(
      get_characteristics, x = .,
      label = names(.), SIMPLIFY = FALSE
    ) %>%
    c(make.row.names = FALSE) %>%
    do.call(rbind, .)
  }

# Implementation ----------------------------------------------------------

  wrapper(clustering) %>%
  data.table::fwrite("zz_tables/0b_clustering.csv")
  
  wrapper(cvd) %>%
  data.table::fwrite("zz_tables/0c_cvd.csv")
  