#' Retrieve non-wear information for an NHANES accelerometer data file
#'
#' This is a wrapper around \code{PhysicalActivity::wearingMarking} that takes
#' into account the non-POSIX formatting of NHANES timestamps
#'
#' @param counts vector of count values (minute-by-minute)
#'
#' @return a vector of wear time information
#' @export
#'
#' @examples
#' data(example_data, package = "SBprofiles")
#' nhanes_nonwear(example_data$PAXINTEN)
nhanes_nonwear <- function(counts) {

  invisible(utils::capture.output(

    result <-
      as.POSIXct("2000-01-01", "UTC") %>%
      seq(by = "1 min", length.out = length(counts)) %>%
      as.character(.) %>%
      data.frame(TimeStamp = ., counts = counts) %>%
      PhysicalActivity::wearingMarking(
        perMinuteCts = 1, cts = "counts",
        getMinuteMarking = TRUE
      ) %>%
      {.$wearing %in% "w"}

  ))

  result

}
