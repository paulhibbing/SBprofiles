## This is set up as a prerequisite to 5b. It takes awhile to run, so it doesn't
## make sense to do over and over again in 5b. The output here is stored in an
## RDS file and then just loaded into 5b. Uncomment and re-run the code if there
## is a need for re-processing

#   rm(list = ls())
#   library(magrittr)
#
#   rstudioapi::getActiveDocumentContext()$path %>%
#   dirname(.) %>%
#   dirname(.) %>%
#   setwd(.)
#
# # Function ----------------------------------------------------------------
#
#   valid_file <- function(
#     file, n, N, min_days = 4, min_hrs = 10
#   ){
#
#     ## Setup
#
#       if (is.na(file)) return(NA_character_)
#
#       cat("\rProcessing file", n, "of", N)
#
#       a <-
#         readRDS(file) #%T>%
#         # {stopifnot(.$PAXMINUT[1] == 0)} This has never failed.
#
#     ## Run through checks
#
#       # Checking for length(unique()) == 1 has never failed
#       if (!a$PAXSTAT[1] %in% 1) return("Unreliable")
#       if (!a$PAXCAL[1]  %in% 1) return("Out_of_Calibration")
#
#       invisible(utils::capture.output(
#         choi_test <-
#           as.POSIXct("2000-01-01", "UTC") %>%
#           seq(by = "1 sec", length.out = nrow(a)) %>%
#           as.character(.) %>%
#           data.frame(TimeStamp = ., axis1 = a$PAXINTEN) %>%
#           {try(PhysicalActivity::wearingMarking(
#             ., perMinuteCts = 1,
#             getMinuteMarking = TRUE
#           ), TRUE)}
#       ))
#
#       if (inherits(choi_test, "try-error")) {
#         warning("Choi failed for ", basename(file))
#         return("Choi_Fail")
#       }
#
#       day_test <-
#         {choi_test$wearing == "w"} %T>%
#         {stopifnot(length(.) == nrow(a))} %>%
#         tapply(a$PAXDAY, sum) %>%
#         {.[. >= (min_hrs*60)]} %>%
#         length(.) %>%
#         {. >= min_days}
#
#       if (!day_test) return("Insufficient_Data")
#
#     ## (else)
#
#       "OK"
#
#   }
#
# # Implementation ----------------------------------------------------------
#
#   options(nwarnings = 1000)
#
#   f <- list.files(
#     "0_Input/rds/activity_monitor",
#     "rds$",
#     full.names = TRUE
#   )
#
#   ids <-
#     basename(f) %>%
#     gsub(".rds$", "", .) %>%
#     as.character(.) %>%
#     as.integer(.)
#
#   mapply(
#     valid_file, f, seq(f), MoreArgs = list(N = length(f)),
#     SIMPLIFY = FALSE, USE.NAMES = FALSE
#   ) %T>%
#   {cat("... DONE!\n")} %>%
#   do.call(c, .) %>%
#   data.frame(
#     id = ids, accel_file = f, accel_status = .,
#     accel_valid = . %in% "OK", stringsAsFactors = FALSE
#   ) %>%
#   saveRDS("0_Input/rds/accelerometer_valid.rds")
#
#   if (!!length(warnings())) { ## No longer needed!
#     sink("0_Input/6a_Choi_Failure_Log.txt")
#       warnings()
#     closeAllConnections()
#   }
