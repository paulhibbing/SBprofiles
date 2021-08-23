#' Determine if wear time algorithm needs to be run
#'
#' If needed, the function will run \code{\link{nhanes_wear}} (with a message)
#'
#' @param df data frame of accelerometer input
#' @inheritParams get_profile
#'
#' @keywords internal
check_wear_time <- function(df, id, wear) {

  if (is.null(wear) & "is_wear" %in% names(df)) {
    stop(
      "Detected a column called `is_wear` with no value passed for ",
      "the `wear` argument.\nThis is not allowed. To fix, either rename ",
      "the current `is_wear` variable\nor rerun the ",
      "call adding this: wear = \"non-wear\"",
      call. = FALSE
    )
  }

  ## Odd logic -- if `wear` is specified, deal with it up front, otherwise,
  ## after conversion to a list via `id_verify`

  if (!is.null(wear)) {

    stopifnot(wear %in% names(df))
    location_of_wear_variable <- which(names(df) == wear)
    stopifnot(length(location_of_wear_variable) == 1)
    names(df)[location_of_wear_variable] <- "is_wear"

  }

  df %<>% id_verify(id)

  if (is.null(wear)) {

    message(
      "Applying Choi non-wear algorithm (separately for",
      " each chunk specified by `id`, if applicable)"
    )

    df %<>%
      lapply("[[", "counts") %>%
      lapply(nhanes_wear) %>%
      {mapply(data.frame, df, is_wear = ., SIMPLIFY = FALSE)}

  }

  df

}
