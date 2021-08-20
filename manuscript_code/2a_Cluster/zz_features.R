variables <-
  c(
    
    "Q10_bout", "Q20_bout", "Q25_bout", "Q30_bout", "Q40_bout",
    "Q50_bout", "Q60_bout", "Q70_bout", "Q75_bout", "Q80_bout",
    "Q90_bout", "IQR", "IDR", "SB_perc", "bout_frequency"#,
    
    # "total_SB_min", "MVPA_min_day","MVPA_perc",
    # 
    # "n_SB_bouts", "n_days", "min_bout_threshold",
    # "total_weartime_min", "weartime_hr_day", 
    # 
    # "adj_total_SB", "mean_SB_bout_min", "adj_mean_SB_bout"
    
  ) %T>%
  {stopifnot(all(. %in% names(d)))}
