## This file has evolved to also include a labeling process for easy location of
## valid accelerometer files. So it's a little misleading to refer to `DVs`, but
## not a big enough problem to warrant changing anything

# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  
  source("0_Input/zz_accel_file_labels.R")

  DVs <- c(
    "SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDEXPRG",
    paste0("BPXSY", 1:4), paste0("BPXDI", 1:4), 
    "BMXBMI", "LBXTR", "LBDTRSI", "LBDLDL", "LBDLDLSI",
    "LBXHDD", "LBDHDDSI", "LBXTC", "LBDTCSI", "LBXGH", "URXPREG",
    "DIQ010", "SMQ040", "BPQ050A",
    paste0("MCQ160", LETTERS[2:6])
  )
  
  labels <- c(
    "id", "sex", "age", "race", "exam_pregnancy",
    paste0("sbp", 1:4), paste0("dbp", 1:4),
    "bmi", "triglycerides", "triglycerides_SI",
    "ldl", "ldl_SI", "hdl", "hdl_SI", "cholesterol",
    "cholesterol_SI", "glycohemoglobin", "pregnancy",
    "diabetes", "smoking", "bp_prescription",
    "chf", "chd", "angina", "mi", "stroke"
  )

# Read and merge DVs ------------------------------------------------------
  
  DVs <-
    list.files(
      "0_input/rds",
      "rds$",
      full.names = TRUE,
      include.dirs = FALSE
    ) %>%
    {.[!grepl("tracker", .)]} %>%
    {.[!grepl("DVs.rds$", .)]} %>%
    lapply(
      function(x, DVs) {
        readRDS(x) %>%
        .[ ,intersect(names(.), DVs)]
      },
      DVs
    ) %>%
    Reduce(
      function(x,y) merge(x,y,all=TRUE), .
    ) %>%
    .[ ,DVs] %>%
    stats::setNames(labels) %>%
    {.[ ,!grepl("_SI$", names(.))]} %T>%
    {rm(labels, envir = globalenv())}

# Label valid accelerometer files -----------------------------------------

  f <- list.files(
    "0_Input/rds/activity_monitor",
    "rds$",
    full.names = TRUE
  )
  
  DVs <-
    basename(f) %>%
    gsub(".rds$", "", .) %>%
    match(DVs$id, .) %>%
    f[.] %>%
    {within(DVs, {accel = .})} %T>%
    {rm(f, envir = globalenv())}
  
  DVs %<>%
    .$accel %>%
    {mapply(
      valid_file, ., seq(.),
      MoreArgs = list(N = length(.)),
      USE.NAMES = FALSE
    )} %>%
    {within(DVs, {accel_valid = .})} %T>%
    structure(., row.names = seq(nrow(.))) %T>%
    {rm(screen_vars, valid_file, envir = globalenv())}
  
  # See `6_Manual_Choi_Failure_Log.txt` for a list of Choi failure cases
  
# Format variables one at a time ------------------------------------------

  ## First remove attributes
  
    for (col in names(DVs)) {
      attributes(DVs[ ,col]) <- NULL
    }

  ## Now take care of variables for which further action *is* needed
  
  ## No further action needed for id, age, bmi, TG,
  ## LDL, HDL, cholesterol, or gHb
  
    DVs$sex %<>% factor(
      labels = c("Male", "Female")
    )
    
    DVs$race %<>% factor(
      labels = c(
        "Mexican American", "Other Hispanic",
        "Non-Hispanic White", "Non-Hispanic Black",
        "Other"
      )
    )
  
    DVs$exam_pregnancy %<>%
      {ifelse(. %in% c(NA, 2), FALSE, NA)}
  
    DVs$sbp1 <-
      DVs[ ,paste0("sbp", 1:4)] %>%
      apply(1, mean, na.rm = TRUE) %>%
      {ifelse(is.nan(.), NA, .)} %>%
      {2 * round(./2)}
    
    DVs$dbp1 <-
      DVs[ ,paste0("dbp", 1:4)] %>%
      apply(1, mean, na.rm = TRUE) %>%
      {ifelse(is.nan(.), NA, .)} %>%
      {2 * round(./2)}
    
    DVs %<>%
      names(.) %>%
      gsub("bp1$", "bp", .) %>%
      stats::setNames(DVs, .) %>%
      {.[ ,!grepl("^[ds]bp[234]{1}$", names(.))]}

    new_names <-
      names(DVs) %>%
      append(., "weight_status", which(. == "bmi"))
    
    DVs %<>%
      within({
        weight_status = sapply(
          bmi, PAutilities::weight_status
        )
      }) %>%
      .[ ,new_names] %T>%
      {rm(new_names, envir = globalenv())}
    
    DVs$pregnancy %<>%
      {ifelse(. %in% c(NA, 2), FALSE, NA)}
  
    DVs$diabetes %<>%
      as.character(.) %>%
      {ifelse(is.na(.), "9", .)} %>%
      sapply(function(x) switch(
        x, "1" = , "3" = TRUE, "2" = FALSE, "9" = NA
      ))
    
    DVs$smoking %<>%
      as.character(.) %>%
      {ifelse(is.na(.), "9", .)} %>%
      sapply(function(x) switch(
        x, "1" = , "2" = TRUE, "3" = FALSE, "9" = NA
      ))
    
    DVs$bp_prescription %<>%
      as.character(.) %>%
      {ifelse(is.na(.), "99", .)} %>%
      sapply(function(x) switch(
        x, "1" = TRUE, "2" = , "99" = FALSE, "9" = NA
      ))
    
    DVs[ ,c("chf", "chd", "angina", "mi", "stroke")] %<>% sapply(
      function(x)  ifelse(x %in% c(NA, 2), FALSE, NA)
    )
    
  ## Save a copy before doing any exclusion
    saveRDS(DVs, "0_Input/rds/raw_DVs.rds")
    
# Exclusions --------------------------------------------------------------
  
  ## Detailed summary of data loss
    # DVs <-
    #     readRDS("0_Input/rds/raw_DVs.rds") %T>%
    #   
    #     {message(
    #       "Removing ", sum(.$age < 30),
    #       " participants on age criterion"
    #     )} %>%
    #     .[.$age >= 30, ]  %T>%
    #   
    #     {message(
    #       "Removing ", sum(!stats::complete.cases(.[ ,names(.)[19:23]])),
    #       " participants with documented CVD"
    #     )} %>%
    #     .[stats::complete.cases(.[ ,names(.)[19:23]]), ] %T>%
    #   
    #     {message(
    #       "Removing ", sum(is.na(.$exam_pregnancy) | is.na(.$pregnancy)),
    #       " files for pregnancy"
    #     )} %>%
    #     {.[!(is.na(.$exam_pregnancy) | is.na(.$pregnancy)), ]} %T>%
    #   
    #     {message(
    #       "Removing ", sum(is.na(.$smoking)),
    #       " participants with unknown smoking status"
    #     )} %>%
    #     .[!is.na(.$smoking), ] %T>%
    #   
    #     {message(
    #       "Removing ", sum(is.na(.$hdl) & is.na(.$cholesterol)),
    #       " participants with unknown HDL and TC"
    #     )} %>%
    #     .[!(is.na(.$hdl) & is.na(.$cholesterol)), ] %T>%
    #   
    #     {message(
    #       "Removing ", sum(is.na(.$sbp)),
    #       " participants with unknown SBP"
    #     )} %>%
    #     .[!is.na(.$sbp), ] %T>%
    #   
    #     {message(
    #       "Removing ", sum(is.na(.$diabetes) | is.na(.$bp_prescription)),
    #       " participants with unknown diabetes or",
    #       " hypertension medication status"
    #     )} %>%
    #     .[!(is.na(.$diabetes) | is.na(.$bp_prescription)), ] %T>%
    #   
    #     {message(
    #       "Removing ", sum(is.na(.$accel)),
    #       " participants who had no monitor data"
    #     )} %>%
    #     .[!is.na(.$accel), ] %T>%
    #   
    #     {message(
    #       "Removing ", sum(!.$accel_valid),
    #       " invalid activity monitor files"
    #     )} %>%
    #     {.[.$accel_valid, ]}
  
  required_vars <- c(
    "age", "sex", "cholesterol", "hdl",
    "sbp", "bp_prescription",
    "diabetes", "smoking",
    "exam_pregnancy", "pregnancy",
    "chf", "chd", "angina", "mi", "stroke"
  )
  
  DVs %<>%
    .[ ,required_vars] %>%
    stats::complete.cases(.) %>%
    DVs[., ] %>%
    {.[ ,!grepl("pregnancy", names(.))]} %>%
    {.[.$age >= 30, ]}

  ## Check on the major variables    
    # sapply(DVs, summary, simplify = FALSE) %>%
    # .[intersect(names(.), required_vars)]
  
  ## Check the other variables
    # sapply(DVs, summary, simplify = FALSE) %>%
    # .[setdiff(names(.), required_vars)]

# Calculate risk and save the dataset -------------------------------------

  DVs %>%
  PAutilities::cvd_risk(
    sex = "sex", age = "age", total_cholesterol = "cholesterol",
    hdl = "hdl", systolic = "sbp", bp_treated = "bp_prescription",
    diabetes = "diabetes", smoker = "smoking", points = FALSE
  ) %>%
  {within(DVs, {cvd_risk = .})} %>%
  saveRDS("0_Input/rds/DVs.rds")
