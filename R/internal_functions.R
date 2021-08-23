#' @name internal_functions
#' @title Functions used internally in the package
#' @details The purpose of \code{check_wear_time} is to determine if the Choi
#'   wear time algorithm needs to be run and, if so, to run it (via
#'   \code{\link{nhanes_wear}}, with a message). To bypass,
#'   run \code{df$is_wear <- TRUE} prior to executing
#'   \code{check_wear_time(df, id, wear = "is_wear")}
#' @keywords internal
NULL

#' @rdname internal_functions
#' @inheritParams get_profile
#' @inheritParams sb_bout_dist
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

#' @rdname internal_functions
#' @keywords internal
id_verify <- function(object, id) {

  if (!is.null(id)) {

    if (!all(
      is.character(id),
      length(id) == 1,
      id %in% names(object)
    )) {
      stop(
        "id must be a character scalar corresponding",
        " to a column name in `object`",
        call. = FALSE
      )
    }

    object %<>%
      split(object[ ,id]) %>%
      stats::setNames(
        ., sapply(., function(x, id) unique(x[ ,id]), id)
      )

  } else {

    object %<>% list(.)

  }

  object

}

#' @rdname internal_functions
#' @keywords internal
counts_verify <- function(object, counts) {

  if (!counts %in% names(object)) {

    stop(
      "`counts` must be a column name in `object`",
      call. = FALSE
    )

  } else {

    stopifnot(sum(names(object) == counts) == 1)

    names(object) %<>% {ifelse(. == counts, "counts", .)}

  }

  object

}

#' @rdname internal_functions
#' @param result an output data frame that may need id-based formatting
#' @keywords internal
id_bind <- function(result, id) {

  if (!is.null(id)) {

    data.frame(
      variable = row.names(result),
      result,
      stringsAsFactors = FALSE,
      row.names = NULL
    ) %>%
      stats::setNames(., gsub("^variable$", id, names(.)))

  } else {

    result

  }

}
