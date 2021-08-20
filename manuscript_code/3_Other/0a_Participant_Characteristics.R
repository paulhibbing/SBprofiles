# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  other <-
    readRDS("0_Input/rds/demographic.rds") %>%
    within({
      INDFMPIR = factor(
        ifelse(INDFMPIR <= 1, "Low", "High"),
        c("Low", "High")
      )
    })
  clustering <-
    readRDS("2a_Cluster/rds/clustered_d.rds") %>%
    within({
      weight_status = sapply(bmi, PAutilities::weight_status)
      sep = other$INDFMPIR[match(id, other$SEQN)]
    }) %>%
    PAutilities::df_reorder("sep", "age")
  cvd <-
    readRDS("2b_Epi/0_Epi_data.rds") %>%
    within({
      sep = other$INDFMPIR[match(id, other$SEQN)]
      cluster = forest_profile
    }) %>%
    PAutilities::df_reorder("sep", "age")

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

  single_wrapper <- function(d, colname = "n_perc") {
    as.list(d) %>%
    mapply(
      get_characteristics, x = .,
      label = names(.), SIMPLIFY = FALSE
    ) %>%
    c(make.row.names = FALSE) %>%
    do.call(rbind, .) %>%
    stats::setNames(., gsub("^n_perc$", colname, names(.)))
  }

  multi_wrapper <- function(d) {
    split(d, d$cluster) %>%
    c(list(total = d), .) %>%
    {mapply(single_wrapper, ., names(.), SIMPLIFY = FALSE)} %>%
    lapply(function(x) if("total" %in% names(x)) x else x[ ,-1]) %>%
    do.call(cbind, .) %>%
    stats::setNames(., gsub("^.*\\.", "", names(.))) %>%
    PAutilities::df_reorder("total", "Prolonged")
  }

# Implementation ----------------------------------------------------------

  multi_wrapper(clustering) %>%
  data.table::fwrite("3_Other/0b_clustering.csv")

  multi_wrapper(cvd) %>%
  data.table::fwrite("3_Other/0c_cvd.csv")
