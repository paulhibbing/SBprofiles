# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  source("0_Input/6a_Exclusion_Functions.R")

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

# Sanity checks -----------------------------------------------------------

  # ## Function
  #
  #   check <- function(age) {
  #
  #     ref <-
  #       readRDS("0_Input/rds/vars.rds") %>%
  #       .[!is.na(.$accel_file), ] %>%
  #       .[.$age >= age,] %>%
  #       .[.$accel_valid, ]
  #
  #     vars <- suppressMessages(
  #       load_and_reduce(
  #         criteria = c(
  #           "accel_exists", "age", "pregnancy",
  #           "chf", "chd", "angina", "mi", "stroke",
  #           "smoking", "cholesterol", "bp", "antihypertensive",
  #           "diabetes", "accel_invalid"
  #         ),
  #         age = age
  #       )
  #     )
  #
  #     setequal(ref$id, vars$id)
  #
  #   }
  #
  # ## Implementation
  #
  #   check(25)
  #   check(30)

# Missing variable-by-variable (Supplemental Table 1) ---------------------

  # load_and_reduce(criteria = c("accel_exists", "age"), age = 25) %>%
  # merge(
  #   readRDS("0_Input/rds/demographic.rds")[ ,c("SEQN", "INDFMPIR")],
  #   by.x = "id", by.y = "SEQN"
  # ) %>%
  # PAutilities::df_reorder("INDFMPIR", "exam_pregnancy") %>%
  # sapply(function(x) {
  #   sum(is.na(x)) %>%
  #   paste0(., " (", round(./length(x)*100), "%)")
  # })

# Data loss in sequence ---------------------------------------------------

  ## NOTE: PCA outliers removed by a subsequent process -- these N's
  ##       are therefore not final, but they are very close

  # clustering <- load_and_reduce(
  #   criteria = c(
  #     "accel_exists", "age", "pregnancy", "accel_invalid"
  #   ), age = 25
  # )
  #
  # epi <-
  #   load_and_reduce(age = 30) %T>%
  #   {stopifnot(all(.$id %in% clustering$id))}
