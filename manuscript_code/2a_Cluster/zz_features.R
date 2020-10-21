variables <-
  c(
    
    "Q10_bout", "Q20_bout", "Q25_bout", "Q30_bout", "Q40_bout",
    "Q50_bout", "Q60_bout", "Q70_bout", "Q75_bout", "Q80_bout",
    "Q90_bout", "IQR", "IDR", "SB_perc", "bouts_weartime"#,
    
    # "SB_1440", "bouts_1440", "MVPA_perc", "MVPA_1440",
    # 
    # "total_SB_raw", "total_MVPA_raw","MVPA_min_per_day_raw", 
    # 
    # "n_bouts", "n_days", "min_bout_threshold",
    # "total_wear_min", "daily_wear_h", 
    # 
    # "total_SB_residual", "mean_SB_bout_raw", "mean_SB_bout_residual",
    # "total_MVPA_residual", "MVPA_min_per_day_residual",
    
  ) %T>%
  {stopifnot(all(. %in% names(d)))}
