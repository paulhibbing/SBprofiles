# Set up, read, and screen ------------------------------------------------

  rm(list = ls())
  library(magrittr)
  
  d <-
    readRDS("0_Input/rds/raw_DVs.rds") %T>%
    {message(
      "Removing ", sum(is.na(.$accel)), " non-files"
    )} %>%
    {.[!is.na(.$accel), ]} %T>%
    {message(
      "Removing ", sum(.$age < 25), " files on age criterion"
    )} %>%
    {.[.$age >= 25, ]} %T>%
    {message(
      "Removing ", sum(is.na(.$exam_pregnancy) | is.na(.$pregnancy)),
      " files for pregnancy"
    )} %>%
    {.[!(is.na(.$exam_pregnancy) | is.na(.$pregnancy)), ]} %>%
    subset(select = -c(exam_pregnancy, pregnancy)) %T>%
    {message(
      "Removing ", sum(!.$accel_valid), " invalid files"
    )} %>%
    {.[.$accel_valid, ]}

# Bout function helpers ---------------------------------------------------

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

      ## This is unnecessary, and the final test has always passed
      # a <-
      #   bouts[ ,c("start_index", "end_index")] %>%
      #   as.list(.)  %>%
      #   {mapply(
      #     seq,
      #     from = .$start_index,
      #     to = .$end_index,
      #     SIMPLIFY = FALSE
      #   )} %>%
      #   do.call(c, .) %>%
      #   unique(.) %>%
      #   a[., ] %T>%
      #   {stopifnot(nrow(.) == sum(bouts$lengths))}
      
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
        id = a$SEQN[1], .,
        IQR = .$Q75_bout - .$Q25_bout,
        IDR = .$Q90_bout - .$Q10_bout,
        total_SB_raw = sum(bouts$lengths),
        n_bouts = nrow(bouts),
        min_bout_threshold = min_bout,
        stringsAsFactors = FALSE
      )} %>%
      merge(d, .)
    
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
      data.frame(d, total_MVPA_raw = ., stringsAsFactors = FALSE)
    
  }
  
# Bout function -----------------------------------------------------------

  get_bouts <- function(
    d, n, N, min_bout = 1,
    probs = c(
      0.1, 0.2, 0.25,
      seq(0.3, 0.7, 0.1),
      0.75, 0.8, 0.9
    ), sb = 100, mvpa = 1952
  ) {
    
    ## Set up, read, and run Choi
    
      timer <- PAutilities::manage_procedure(
        "Start", "\rProcessing row", n, "of", N
      )
    
      a <- readRDS(d$accel)
      
      invisible(utils::capture.output(
        a$is_wear <-
          as.POSIXct("2000-01-01", "UTC") %>%
          seq(by = "1 sec", length.out = nrow(a)) %>%
          as.character(.) %>%
          data.frame(TimeStamp = ., counts = a$PAXINTEN) %>%
          PhysicalActivity::wearingMarking(
            perMinuteCts = 1, cts = "counts",
            getMinuteMarking = TRUE
          ) %>%
          {.$wearing %in% "w"}
      ))
  
    ## Determine valid days (and total wear time)
      
      valid_days <-
        tapply(a$is_wear, a$PAXDAY, sum) %>%
        {. >= 600} %>%
        {names(.)[.]} %>%
        as.character(.) %>%
        as.numeric(.)
      
      valid_indices <-
        (a$PAXDAY %in% valid_days) %>%
        {seq(.)[.]}
      
      total_wear_min <-
        tapply(a$is_wear, a$PAXDAY, sum) %>%
        .[names(.) %in% valid_days] %>%
        sum(.)
      
    ## Get bout information
      
      d %>%
      sb_bouts(a, sb, min_bout, valid_indices, probs) %>%
      mvpa_bouts(a, mvpa, valid_indices) %>%
      within({
        total_wear_min = total_wear_min
        n_days = length(valid_days)
      }) %>%
      within({
        bouts_weartime = n_bouts / total_wear_min
        daily_wear_h = (total_wear_min / n_days) / 60
      }) %>%
      within({
        bouts_1440 = round(bouts_weartime * 1440, 0)
      })
    
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
      
    residual_adjust("total_SB_raw", "total_wear_min", "total_SB_residual") %>%
    within({
      SB_perc = total_SB_raw / total_wear_min
    }) %>%
    within({
      mean_SB_bout_residual = total_SB_residual / n_bouts
      mean_SB_bout_raw = total_SB_raw / n_bouts
      SB_1440 = round(SB_perc * 1440, 0)
    }) %>%  
      
    residual_adjust("total_MVPA_raw", "total_wear_min", "total_MVPA_residual")  %>%
    within({
      MVPA_perc = total_MVPA_raw / total_wear_min
    }) %>%
    within({
      MVPA_min_per_day_residual = total_MVPA_residual / n_days
      MVPA_min_per_day_raw = total_MVPA_raw / n_days
      MVPA_1440 = round(MVPA_perc * 1440, 0)
    })
    
  }
  
# Implementation ----------------------------------------------------------

  print("Bout threshold 1 min (i.e., no threshold)")
    d %>%
    nrow(.) %>%
    seq(.) %>%
    split(d, .) %>%
    {mapply(
      get_bouts, ., seq(.),
      MoreArgs = list(N = nrow(d)),
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )} %>%
    final_format(.) %>%
    saveRDS("1_Bout_Data/d1_final.rds")
  
  print("Bout threshold 5 min")
    d %>%
    nrow(.) %>%
    seq(.) %>%
    split(d, .) %>%
    {mapply(
      get_bouts, ., seq(.),
      MoreArgs = list(
        N = nrow(d), min_bout = 5
      ),
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )} %>%
    final_format(.) %>%
    saveRDS("1_Bout_Data/d5_final.rds")
