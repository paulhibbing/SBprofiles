# Set up, read, and screen ------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  source("0_Input/6a_Exclusion_Functions.R")

  d <-
    load_and_reduce(
      criteria = c(
        "accel_exists", "age", "pregnancy", "accel_invalid"
      ),
      age = 25
    ) %T>%
    {rm(list = ls(envir = globalenv()), envir = globalenv())}

  source("1_Bout_Data/zz_functions.R")

# Bout function -----------------------------------------------------------

  get_bouts <- function(
    d, n, N, min_bout = 1,
    probs = c(
      0.1, 0.2, 0.25,
      seq(0.3, 0.7, 0.1),
      0.75, 0.8, 0.9
    ), sb = 100, mvpa = 1952
  ) {

    ## Read file and run Choi algorithm

      a <- readRDS(d$accel_file)

      invisible(utils::capture.output(
        a$is_wear <-
          as.POSIXct("2000-01-01", "UTC") %>%
          seq(by = "1 min", length.out = nrow(a)) %>%
          as.character(.) %>%
          data.frame(TimeStamp = ., axis1 = a$PAXINTEN) %>%
          PhysicalActivity::wearingMarking(
            perMinuteCts = 1, getMinuteMarking = TRUE
          ) %>%
          {.$wearing %in% "w"}
      ))

    ## Determine valid days and wear time

      valid_days <-
        tapply(a$is_wear, a$PAXDAY, sum) %>%
        {. >= 600} %>%
        {names(.)[.]} %>%
        as.character(.) %>%
        as.numeric(.)

      valid_indices <-
        (a$PAXDAY %in% valid_days) %>%
        {seq(.)[.]}

      total_weartime_min <-
        tapply(a$is_wear, a$PAXDAY, sum) %>%
        .[names(.) %in% valid_days] %>%
        sum(.)

    ## Get bout information

      d %>%
      within({
        weartime_hr_day = total_weartime_min / length(valid_days) / 60
        total_weartime_min = total_weartime_min
        n_days = length(valid_days)
      }) %>%
      sb_bouts(a, sb, min_bout, valid_indices, probs) %>%
      mvpa_bouts(a, mvpa, valid_indices)

  }

# Implementation ----------------------------------------------------------

  print("Bout threshold 1 min (i.e., no threshold)")
    d %>%
    nrow(.) %>%
    seq(.) %>%
    split(d, .) %>%
    {mapply(
      get_bouts, ., seq(.),
      MoreArgs = list(N = nrow(d)),
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )} %>%
    final_format(.) %>%
    saveRDS("1_Bout_Data/d1_final.rds")

  print("Bout threshold 5 min")
    d %>%
    nrow(.) %>%
    seq(.) %>%
    split(d, .) %>%
    {mapply(
      get_bouts, ., seq(.),
      MoreArgs = list(
        N = nrow(d), min_bout = 5
      ),
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )} %>%
    final_format(.) %>%
    saveRDS("1_Bout_Data/d5_final.rds")
