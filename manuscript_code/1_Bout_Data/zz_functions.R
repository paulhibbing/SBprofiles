# Bout Retrieval ----------------------------------------------------------

sb_bouts <- function(d, a, sb, min_bout, valid_indices, probs) {
  
  ## Determine all SB bouts
  
  bouts <-
    {a$PAXINTEN <= sb} %>%
    paste(a$is_wear) %>%
    PAutilities::index_runs(.) %>%
    within({values = as.character(values)}) %>%
    .[.$values=="TRUE TRUE", ] %>%
    .[.$lengths >= min_bout, ]
  
  ## Exclude bouts that overlap with invalid days
  
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
  
  ## Assemble features
  
  bouts$lengths %>%
  quantile(probs = probs) %>%
  t(.) %>%
  data.frame(.) %>%
  stats::setNames(
    ., gsub("\\.$", "_bout", names(.))
  ) %>%
  stats::setNames(
    ., gsub("^X", "Q", names(.))
  ) %>%
  {data.frame(
    id = a$SEQN[1],
    n_SB_bouts = nrow(bouts),
    min_bout_threshold = min_bout,
    total_SB_min = sum(bouts$lengths),
    .,
    IQR = .$Q75_bout - .$Q25_bout,
    IDR = .$Q90_bout - .$Q10_bout,
    stringsAsFactors = FALSE
  )} %>%
  merge(d, .) %>%
  within({
    bout_frequency = n_SB_bouts / total_weartime_min * 60
    mean_SB_bout_min = total_SB_min / n_SB_bouts
    SB_perc = total_SB_min / total_weartime_min
    SB_hr_day = total_SB_min / n_days / 60
  }) %>%
  PAutilities::df_reorder(
    c("SB_hr_day", "mean_SB_bout_min", "SB_perc"), "total_SB_min"
  ) %>%
  cut_bouts(bouts) %>%
  usual_bout_duration(bouts) %>%
  fragmentation_index(a, bouts) %>%
  gini(bouts) %>%
  alpha(bouts)
  
}

mvpa_bouts <- function(d, a, mvpa, valid_indices) {
  
  ## Determine all bouts
  
  bouts <-
    {a$PAXINTEN >= mvpa} %>%
    paste(a$is_wear) %>%
    PAutilities::index_runs(.) %>%
    within({values = as.character(values)}) %>%
    .[.$values=="TRUE TRUE", ]
  
  ## Exclude bouts that overlap with invalid days
  
  if (nrow(bouts) > 0) {
    
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
    
  }
  
  ## Finish up
  
  sum(bouts$lengths) %>%
  data.frame(d, total_MVPA_raw = ., stringsAsFactors = FALSE) %>%
  within({
    MVPA_perc = total_MVPA_raw / total_weartime_min
    MVPA_min_day = total_MVPA_raw / n_days
  })
  
}

# Duration Encoding -------------------------------------------------------

cut_bouts <- function(d, bouts) {
  data.frame(
    d,
    sb_0_14 = sum(ifelse(
      bouts$lengths < 15, bouts$lengths, 0
    )) / d$n_days / 60,
    sb_15_29 = sum(ifelse(
      bouts$lengths >= 15 & bouts$lengths < 30, bouts$lengths, 0
    )) / d$n_days / 60,
    sb_30_Inf = sum(ifelse(
      bouts$lengths >= 30, bouts$lengths, 0
    )) / d$n_days / 60
  ) %T>%
  {stopifnot(isTRUE(all.equal(
    sum(rev(.)[ ,1:3]), sum(bouts$lengths) / d$n_days / 60,
    scale = 1, tolerance = 1/60/10
  )))}
}

# Duration Summarizing ----------------------------------------------------

usual_bout_duration <- function(d, bouts) {
  
  df <-
    table(bouts$lengths) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(., gsub("^Var1$", "l", names(.))) %>%
    within({
      l = as.numeric(as.character(l))
      cumprop = 0
      prop = 0
      tot.l = l * Freq
      total_time = sum(tot.l)
      prop = tot.l/total_time
      total_time = NULL
      cumprop = cumsum(prop)
    }) %T>%
    {stopifnot(identical(order(.$l), seq(nrow(.))))}
  
  empirical <-
    abs(df$cumprop - 0.5) %>%
    which.min(.) %>%
    df$l[.]
  
  stats::nls(
    cumprop ~ l^n/(l^n+W50^n),
    data = df,
    start = c(n=1,W50=empirical)
  ) %>%
  coef(.) %>%
  .["W50"] %>%
  unname(.) %>%
  data.frame(
    d, ubd_empirical = empirical, ubd_predicted = .
  )
  
}

# SB Breaks ---------------------------------------------------------------

fragmentation_index <- function(d, a, bouts) {
  bouts %>%
  within({
    `break` = ifelse(end_index == nrow(a), 0, 1)
  }) %>%
  {sum(.$`break`) / sum(.$lengths) * 60} %>%
  data.frame(d, fragmentation_index = .)
}

# Complex Metrics ---------------------------------------------------------

gini <- function(d, bouts) {
  data.frame(d, gini = DescTools::Gini(bouts$lengths))
}

alpha <- function(d, bouts) {
  bouts$lengths %>%
  sapply(., function(xi, xm) log(xi/xm), xm = min(.)) %>%
  sum(.) %>%
  {data.frame(
    alpha = 1 + nrow(bouts)/.
  )} %>%
  within({
    alpha_se = (alpha - 1) / sqrt(nrow(bouts))
  }) %>%
  data.frame(d, .)
}

# Adjustment and formatting functions -------------------------------------

residual_adjust <- function(d, variable, confounder, label) {
  
  if ("zznewvariable" %in% names(d)) stop(
    "`d` cannot have a variable called `zznewvariable`"
  )
  
  cat("\n")
  
  paste(
    "Performing residual adjustment for", variable,
    "based on", confounder
  ) %>%
  print(.)
  
  paste0(variable, " ~ ", confounder) %>%
  as.formula(.) %>%
  lm(d) %>%
  {.$residuals + mean(.$fitted.values)} %>%
  {within(d, {zznewvariable = .})} %>%
  stats::setNames(
    ., gsub("^zznewvariable$", label, names(.))
  )
  
}

final_format <- function(d) {
  
  c(d, make.row.names = FALSE) %>%
  do.call(rbind, .) %>%
  
  residual_adjust(
    "SB_hr_day", "weartime_hr_day", "adj_total_SB"
  ) %>%
  residual_adjust(
    "mean_SB_bout_min", "weartime_hr_day", "adj_mean_SB_bout"
  ) %>%

  residual_adjust("sb_0_14", "weartime_hr_day", "adj_sb_0_14") %>%
  residual_adjust("sb_15_29", "weartime_hr_day", "adj_sb_15_29") %>%
  residual_adjust("sb_30_Inf", "weartime_hr_day", "adj_sb_30_Inf") %>%
  residual_adjust("Q50_bout", "weartime_hr_day", "adj_median_sb_bout") %>%
  residual_adjust("MVPA_min_day", "weartime_hr_day", "adj_MVPA")
  
}
