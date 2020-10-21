#' Convert an object of class `confusionMatrix` to a data frame containing the
#' same information
#'
#' This operation is useful for convenient exporting and reporting in tables
#'
#' @return A data frame containing the information from the `confusionMatrix`
#'   object
#' @keywords internal
#'
summary.confusionMatrix <- function(object, ...) {
  # object <- cm

  var_names <- names(dimnames(object$table))
  instances <- reshape2::dcast(
    data.frame(object$table, stringsAsFactors = FALSE),
    eval(parse(text = var_names[1])) ~
      eval(parse(text = var_names[2])),
    value.var = "Freq"
  )

  names(instances) <-
    c(
      var_names[1],
      paste(
        var_names[2], names(instances)[-1], sep = "_"
      )
  )

  overall <-
    data.frame(
      t(object$overall),
      stringsAsFactors = FALSE
  )

  byClass <-
    data.frame(
      object$byClass,
      stringsAsFactors = FALSE,
      row.names = NULL
  )

  data.frame(
    instances,
    overall,
    byClass,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}
