# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  source("0_Input/6a_Exclusion_Functions.R")

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
  #           "diabetes", "accel_valid"
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
  # sapply(function(x) {
  #   sum(is.na(x)) %>%
  #   paste0(., " (", round(./length(x)*100), "%)")
  # })

# Data loss in sequence ---------------------------------------------------

  # clustering <- load_and_reduce(
  #   criteria = c(
  #     "accel_exists", "age", "pregnancy", "accel_valid"
  #   ), age = 25
  # )
  # 
  # epi <-
  #   load_and_reduce(age = 30) %T>%
  #   {stopifnot(all(.$id %in% clustering$id))}
  