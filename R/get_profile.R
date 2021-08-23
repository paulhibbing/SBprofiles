#' Implement the sedentary profiles
#'
#' @param object input (either data frame or output from
#'   \code{\link{sb_bout_dist}})
#' @param ... further arguments passed to \code{\link{sb_bout_dist}} (used only
#'   in data frame method)
#'
#' @return one or more profile assignments
#' @export
#'
#' @examples
#' data(example_data, package = "SBprofiles")
#' get_profile(example_data, counts = "PAXINTEN")
#' get_profile(
#'   example_data, counts = "PAXINTEN", method = "randomForest"
#' )
#' get_profile(example_data, id = "PAXDAY", counts = "PAXINTEN")
#' get_profile(
#'   example_data, id = "PAXDAY",
#'   counts = "PAXINTEN", method = "decisionTree"
#' )
get_profile <- function(object, ...) {

  requireNamespace("tree", quietly = TRUE)
  requireNamespace("randomForest", quietly = TRUE)

  UseMethod("get_profile", object)

}

#' @rdname get_profile
#' @param method character. The model(s) to use for the prediction
#' @export
get_profile.bout <- function(
  object, method = c("both", "decisionTree", "randomForest"), ...
) {

  method <- match.arg(method)

  if (inherits(object, "bout1")) {

    dt <- stats::predict(.$tree1, object, "class")
    rf <-
      stats::predict(.$forest1, object) %>%
      unname(.)

  } else if (inherits(object, "bout5")) {

    dt <- stats::predict(.$tree5, object, "class")
    rf <-
      stats::predict(.$forest5, object) %>%
      unname(.)

  } else {

    stop(
      "cannot predict SB profile because object does",
      " not have `bout1` or `bout5` class!", call. = FALSE
    )

  }

  both <-
    list(dt, rf) %>%
    stats::setNames(c("decisionTree", "randomForest"))

  switch(
    method,
    "both" = both,
    "decisionTree" = dt,
    "randomForest" = rf,
    NULL
  )

}

#' @rdname get_profile
#' @param id character scalar (optional). Column name on which to divide
#'   \code{object} (if a data frame) into a list of separate objects
#' @param counts character scalar. Column name of the variable to use when
#'   classifying sedentary behavior (and wear time, depending on the function)
#' @param wear character scalar [optional]. Column name of the variable to use
#'   for determining wear time (logical vector with \code{TRUE} for wear time
#'   minutes). If no value is provided, \code{\link{nhanes_wear}} is invoked on
#'   the \code{counts} column
#' @param sb integer. The cut point to use for classifying sedentary behavior
#' @param valid_indices integer vector (optional) specifying which indices of
#'   \code{is_sb} and {is_wear} correspond to valid measurement days (e.g. with
#'   10+ hours of wear time on 4+ days)
#' @export
get_profile.data.frame <- function(
  object, method = c("both", "decisionTree", "randomForest"),
  id = NULL, counts = NULL, wear = NULL, sb = 100, valid_indices = NULL, ...
) {

  method <- match.arg(method)

  object %>%
  df_check_format(counts, valid_indices, id, wear) %>%
  lapply(
    function(x, sb, ...) {sb_bout_dist(
      df = NULL,
      is_sb = x$counts <= sb,
      is_wear = x$is_wear,
      valid_indices = which(x$valid_index),
      ...
    )},
    sb, ...
  ) %>%
  lapply(get_profile, method) %>%
  lapply(function(x, method) {
    if (length(x) > 1)
      do.call(data.frame, x)
    else
      stats::setNames(data.frame(x), method)
  }, method) %>%
  id_bind(id)

}

#' @rdname get_profile
#' @export
get_profile.list <- function(
  object, method = c("both", "decisionTree", "randomForest"), ...
) {

  stopifnot(sapply(object, inherits, c("bout1", "bout5")))
  method <- match.arg(method)

  object %<>% lapply(get_profile, method = method)

  if (method == "both") {
    object %>%
    lapply(c, stringsAsFactors = FALSE) %>%
    lapply(do.call, what = data.frame) %>%
    do.call(rbind, .) %>%
    as.list(.)
  } else {
    unlist(object, use.names = FALSE)
  }

}
