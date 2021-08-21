# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

# Helper functions --------------------------------------------------------

  get_variables <- function(info, label) {

    row_labs = c(
      "SAS Label: ", "English Text: ",
      "English Instructions: ", "Target: ",
      "Hard Edits: "
    )

    fields <-
      xml2::xml_find_all(info, "//dt") %>%
      rvest::html_text(.) %T>%
      {stopifnot(
        .[1] == "Variable Name: ",
        all(. %in% c("Variable Name: ", row_labs))
      )}

    values <-
      xml2::xml_find_all(info, "//dd") %>%
      rvest::html_text(.) %T>%
      {stopifnot(.[1] == "SEQN")}

    data.frame(
      dataset = label,
      field = fields,
      value = values,
      stringsAsFactors = FALSE
    ) %>%
    split(., cumsum(.$field == "Variable Name: ")) %>%
    lapply(function(x) rbind(x, "")) %>%
    c(make.row.names = FALSE) %>%
    do.call(rbind, .)

  }

  get_tables <- function(x, label, varname) {

    initial <- rvest::html_nodes(x, "table")

    if (!length(initial)) return(NULL)

    rvest::html_table(initial) %>%
    {.[[1]]} %>%
    data.frame(
      dataset = label, variable = varname, .,
      stringsAsFactors = FALSE
    ) %>%
    rbind("")

  }

  get_codebook <- function(url, label, ...) {

    cat("\nScraping", basename(url))

    info <-
      xml2::read_html(url) %>%
      rvest::html_node("body")

    vars <- get_variables(info, label)

    varnames <-
      {vars$field == "Variable Name: "} %>%
      vars$value[.]

    tables <-
      xml2::xml_find_all(info, "//div[contains(@class, 'pagebreak')]") %>%
      split(., seq(.)) %>%
      stats::setNames(names(vars[ ,-1])) %>%
      mapply(get_tables, ., label, varnames, SIMPLIFY = FALSE) %>%
      c(make.row.names = FALSE) %>%
      do.call(rbind, .)

    data.table::fwrite(vars, "0_Input/codebook_vars.csv", ...)
    data.table::fwrite(tables, "0_Input/codebook_values.csv", ...)

  }

# Implementation ----------------------------------------------------------

  ## Note: For our analyses, we are only using variables that occurred
  ## in both the 2003-2004 cycle and the 2005-2006 cycle. Therefore, the
  ## approach here is to retrieve documentation from 2003-2004 only. The
  ## exception is accelerometer documentation (PAXRAW), since an extra
  ## variable (PAXSTEP) was included for each participant in 2005-2006.
  ## Apart from that, a couple other considerations: First, While the
  ## *analyses* will be focused on variables that were common to both
  ## cycles, this *documentation* will pull *all* variables from 2003-2004,
  ## even the ones that were not repeated in 2005-2006. Additionally, the
  ## items that *were* included in both cycles may were not necessarily
  ## delivered identically (e.g., different survey response options).
  ## So, take this documentation with a grain of salt -- it should give
  ## a good overview of the data, but not a comprehensive guide. You'll
  ## need to peruse the website (or modify this code) if that's what
  ## you're after.

  urls <- c(
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPX_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAXRAW_D.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13AM_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/UC_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/SMQ_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPQ_C.htm",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/MCQ_C.htm"
  )

  labels <- c(
    "Demographic", "Blood Pressure", "Body Measures", "Activity Monitor",
    "LDL/Triglycerides", "HDL/Total Cholesterol", "Glycohemoglobin",
    "Pregnancy", "Diabetes", "Smoking", "Hypertension", "Medical"
  )

  append <-
    rep(TRUE, length(urls) - 1) %>%
    c(FALSE, .)

  mapply(get_codebook, urls, labels, append = append, SIMPLIFY = FALSE) %>%
  invisible(.)
