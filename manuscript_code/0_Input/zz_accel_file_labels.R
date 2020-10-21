screen_vars <- function(a) {
  a[ ,c("PAXSTAT", "PAXCAL")] %>%
  as.list(.) %>%
  lapply(unique) %T>%
  {stopifnot(
    all(sapply(., length) == 1)
  )} %>%
  unlist(.) %>%
  {ifelse(. %in% 1, TRUE, FALSE)} %>%
  all(.)
}

valid_file <- function(
  file, n, N, min_days = 4, min_hrs = 10
){
  
  if (is.na(file)) return(NA)
  
  timer <- PAutilities::manage_procedure(
    "Start", "\nProcessing file", n, "of", N
  )
  
  a <-
    readRDS(file) %T>%
    {stopifnot(.$PAXMINUT[1] == 0)}
  
  cat("\n...Checking reliability and calibration")
  
  if (!screen_vars(a)) {
    PAutilities::manage_procedure(
      "End", timer = timer
    )
    return(FALSE)
  }
  
  cat("\n...Running Choi")
  
  invisible(utils::capture.output(
    a$is_wear <-
      as.POSIXct("2000-01-01", "UTC") %>%
      seq(by = "1 sec", length.out = nrow(a)) %>%
      as.character(.) %>%
      data.frame(TimeStamp = ., counts = a$PAXINTEN) %>%
      {try(PhysicalActivity::wearingMarking(
        ., perMinuteCts = 1, getMinuteMarking = TRUE
      ), TRUE)} %>%
      {
        if ("try-error" %in% class(.)) {
          warning(
            "Choi failed for ", basename(file),
            call. = FALSE
          )
          NA
        } else {
          {.$wearing %in% "w"}  
        }
      }
  ))
  
  if (all(is.na(a$is_wear))) {
    PAutilities::manage_procedure(
      "End", timer = timer
    )
    return(FALSE)
  }
  
  cat("\n...Checking valid days")
  
  tapply(a$is_wear, a$PAXDAY, sum) %>%
  {.[. >= (min_hrs*60)]} %>%
  length(.) %>%
  {. >= min_days} %T>%
  {PAutilities::manage_procedure(
    "End", timer = timer
  )}
  
}
