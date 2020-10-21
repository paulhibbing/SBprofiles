# Generic and methods -----------------------------------------------------

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
    "randomForest" = rf
  )

}

#' @rdname get_profile
#' @param id character scalar (optional). Column name on which to divide
#'   \code{object} (if a data frame) into a list of separate objects
#' @param counts character scalar. Column name of the variable to use when
#'   classifying sedentary behavior
#' @param sb integer. The cut point to use for classifying sedentary behavior
#' @export
get_profile.data.frame = function(
  object, method = c("both", "decisionTree", "randomForest"),
  id = NULL, counts, sb = 100, ...
) {

  method <- match.arg(method)

  if (!counts %in% names(object)) {

    stop(
      "`counts` must be a column name in `object`",
      call. = FALSE
    )

  } else {

    names(object) %<>% {ifelse(. == counts, "counts", .)}

  }

  if (!is.null(id)) {

    id %<>% id_verify(object)
    object %<>% split(object[ ,id])

  } else {

    object %<>% list(.)

  }

  lapply(object, function(x, sb, ...) {
    sb_bout_dist(
      x$counts <= sb,
      nhanes_wear(x$counts),
      ...
    )
  }, sb, ...) %>%
  lapply(get_profile, method) %>%
  lapply(function(x, method) {
    if (length(x) > 1)
      do.call(data.frame, x)
    else
      stats::setNames(data.frame(x), method)
  }, method) %>%
  do.call(rbind, .)

}

# Helper function(s) -------------------------------------------------------

  id_verify <- function(id, object) {

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

    id

  }
