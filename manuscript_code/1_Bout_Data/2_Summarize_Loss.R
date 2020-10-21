## SETUP ####

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
    subset(select = -c(exam_pregnancy, pregnancy)) %>%
    .[!.$accel_valid, ]
  
## Choi
  
  choi <-
    readLines("0_Input/6_Manual_Choi_Failure_Log.txt") %>%
    .[-1] %>%
    gsub("^[0-9]+: Choi failed for ", "", .) %>%
    gsub(".rds$", "", .) %>%
    as.character(.) %>%
    as.numeric(.) %>%
    intersect(d$id)
  
  d %<>%
    {!.$id %in% choi} %T>%
    {message(
      "Removing ", length(choi),
      " files with failed Choi algorithm"
    )} %>%
    d[., ]
  
## PAXSTAT, PAXCAL, or CHOI

  # stat <- character()
  # cal <- character()
  # weartime <- character()
  # 
  # for (i in 1:nrow(d)) {
  #   
  #   cat("\r", i, "of", nrow(d))
  #   
  #   a <-
  #     d$accel[i] %>%
  #     readRDS(.)
  #   
  #   a[1,c("PAXSTAT", "PAXCAL")] %>%
  #   as.vector(.) %>%
  #   {. %in% 1:2} %>%
  #   {
  #     if (all(!.)) {
  #       warning(
  #         "Nonstandard PAXSTAT and PAXCAL for ",
  #         d$id[i], call. = FALSE
  #       )
  #     } else if (!.[1]) {
  #       warning(
  #         "Nonstandard PAXSTAT for ",
  #         d$id[i], call. = FALSE
  #       )
  #     } else if (!.[2]) {
  #       warning(
  #         "Nonstandard PAXCAL for ",
  #         d$id[i], call. = FALSE
  #       )
  #     }
  #   }
  #   
  #   if (!all(a$PAXSTAT==1)) {
  #     stat %<>% append(i)
  #   } else if (!all(a$PAXCAL==1)) {
  #     cal %<>% append(i)
  #   } else {
  #     
  #     invisible(utils::capture.output(
  #       a$is_wear <-
  #         as.POSIXct("2000-01-01", "UTC") %>%
  #         seq(by = "1 sec", length.out = nrow(a)) %>%
  #         as.character(.) %>%
  #         data.frame(TimeStamp = ., counts = a$PAXINTEN) %>%
  #         PhysicalActivity::wearingMarking(
  #           perMinuteCts = 1, getMinuteMarking = TRUE
  #         ) %>%
  #         {.$wearing %in% "w"}
  #     ))
  #     
  #     daytest <-
  #       tapply(a$is_wear, a$PAXDAY, sum) %>%
  #       {. >= 600} %>%
  #       {sum(.) >= 4}
  #     
  #     if (!daytest) {
  #       weartime %<>% append(i)
  #     } else {
  #       stop("Don\'t know why this file was excluded")
  #     }
  #     
  #   }
  # }
  
    # > length(cal)
    # [1] 293
    # > length(stat)
    # [1] 174
    # > length(weartime)
    # [1] 1199
  
    # Warning message:
    #   Nonstandard PAXCAL for 29507
    