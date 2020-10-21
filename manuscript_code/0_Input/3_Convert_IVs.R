## Setup

  rm(list = ls())
  library(magrittr)
  
  tracker <- readRDS("0_Input/rds/tracker.rds")
  
  out_dir <- "0_Input/rds/activity_monitor"
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

## Processing Function
  
  save_monitor <- function(x, out_dir, data) {
    
    timer <- PAutilities::manage_procedure(
      "Start", "\n\nProcessing", x
    )
    
    out_file <-
      paste0(x, ".rds") %>%
      file.path(out_dir, .)
    
    if (file.exists(out_file)) {
      cat("\n...Skipping (already exists)")
      return(NULL)
    }
    
    subset(data, SEQN == x) %>%
    saveRDS(out_file) %T>%
    {PAutilities::manage_procedure("End", timer = timer)}
    
  }

## Process 2003-2004 (time intensive - must manually uncomment to run)
  
  # c <- SASxport::read.xport(tracker$C[8])
  # 
  # beepr::beep(8)
  # svDialogs::dlg_message(
  #   paste("Prepare to process", length(unique(d$SEQN)), "participants"),
  #   "ok"
  # )
  # 
  # unique(c$SEQN) %>%
  # lapply(save_monitor, out_dir, c)
  # 
  # rm(c)
  # gc()
  
## Process 2005-2006 (time intensive - must manually uncomment to run)
  
  # d <- SASxport::read.xport(tracker$D[8])
  # 
  # beepr::beep(8)
  # svDialogs::dlg_message(
  #   paste("Prepare to process", length(unique(d$SEQN)), "participants"),
  #   "ok"
  # )
  # 
  # unique(d$SEQN) %>%
  # lapply(save_monitor, out_dir, d)
  # 
  # rm(d)
  # gc()
  