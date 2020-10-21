#' Extract features to summarize a participant's bout duration distribution
#'
#' @param df data frame input (for vectorized operations)
#' @param is_sb logical vector reflecting minute-by-minute classifications
#'   (\code{TRUE} for sedentary behavior and \code{FALSE} for non-sedentary
#'   behavior)
#' @param is_wear logical vector reflecting minute-by-minute wear
#'   classifications (\code{TRUE} for wearing and \code{FALSE} for not wearing)
#' @param min_bout integer scalar. Threshold for a run to qualify as a bout
#' @param valid_indices integer vector (optional) specifying which indices of
#'   \code{is_sb} and {is_wear} correspond to valid measurement days (e.g. with
#'   10+ hours of wear time on 4+ days)
#' @param probs numeric vector. Percentiles to calculate (all values must be
#'   between 0 and 1)
#'
#' @return A bout object (data frame of distribution features)
#' @inheritParams get_profile
#'
#' @export
#'
#' @examples
#' data(example_data, package = "SBprofiles")
#' sb_bout_dist(
#'   is_sb = example_data$PAXINTEN <= 100,
#'   is_wear = nhanes_wear(example_data$PAXINTEN)
#' )
#' sb_bout_dist(
#'   example_data, id = "PAXDAY", counts = "PAXINTEN"
#' )
sb_bout_dist <- function(
  df = NULL, is_sb, is_wear, min_bout = 5,
  valid_indices = NULL, id = NULL, counts, sb = 100,
  probs = c(
    0.1, 0.2, 0.25,
    seq(0.3, 0.7, 0.1),
    0.75, 0.8, 0.9
  )
) {

  if (is.null(df)) {

    sb_bout_dist_default(
      is_sb, is_wear, min_bout, valid_indices, probs
    )

  } else {

    sb_bout_dist_df(
      df, counts, id, sb, min_bout, valid_indices, probs
    )

  }

}

#' @rdname internal_functions
#' @inheritParams sb_bout_dist
#' @keywords internal
sb_bout_dist_default <- function(
  is_sb, is_wear, min_bout, valid_indices, probs
) {

  ## Determine all SB bouts

    bouts <-
      paste(is_sb, is_wear) %>%
      PAutilities::index_runs(.) %>%
      within({values = as.character(values)}) %>%
      .[.$values=="TRUE TRUE", ] %>%
      .[.$lengths >= min_bout, ]

  ## If applicable, exclude bouts that overlap with invalid days
  ## Also calculate total wear time

    total_wear_min <- sum(is_wear)

    if (!is.null(valid_indices)) {

      bouts %<>%
        nrow(.) %>%
        seq(.) %>%
        split(bouts, .) %>%
        sapply(function(x, valid_indices) {
          seq(x$start_index, x$end_index) %>%
          {. %in% valid_indices} %>%
          all(.)
        }, valid_indices = valid_indices) %>%
        bouts[., ]

      total_wear_min <-
        which(is_wear) %>%
        intersect(valid_indices) %>%
        length(.)

    }

  ## Assemble features

    bouts$lengths %>%
    stats::quantile(probs = probs) %>%
    t(.) %>%
    data.frame(.) %>%
    stats::setNames(
      ., gsub("\\.$", "_bout", names(.))
    ) %>%
    stats::setNames(
      ., gsub("^X", "Q", names(.))
    ) %>%
    data.frame(
      IQR = .$Q75_bout - .$Q25_bout,
      IDR = .$Q90_bout - .$Q10_bout,
      total_SB_raw = sum(bouts$lengths),
      n_bouts = nrow(bouts),
      total_wear_min = total_wear_min,
      stringsAsFactors = FALSE
    ) %>%
    within({
      min_bout_threshold = min_bout
      bouts_weartime = n_bouts / total_wear_min
      SB_perc = total_SB_raw / total_wear_min
    }) %>%
    structure(
      .,
      class = append(
        class(.),
        c("bout", paste0("bout", min_bout)),
        0
      )
    )

}

#' @rdname internal_functions
#' @inheritParams sb_bout_dist
#' @keywords internal
sb_bout_dist_df <- function(
  df, counts, id, sb, min_bout, valid_indices, probs
) {

  df %>%
  counts_verify(counts) %>%
  id_verify(id) %>%
  lapply(
    function(x, sb, min_bout, valid_indices, probs) {
      sb_bout_dist_default(
        is_sb = x$counts <= sb,
        is_wear = nhanes_wear(x$counts),
        min_bout = min_bout,
        valid_indices = valid_indices,
        probs = probs
      )},
    sb, min_bout, valid_indices, probs
  ) %>%
  do.call(rbind, .) %>%
  id_bind(id)

}
