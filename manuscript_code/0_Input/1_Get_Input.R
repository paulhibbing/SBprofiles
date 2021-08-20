# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

# XPT Files ---------------------------------------------------------------

  get_xpt <- function(url, destfile) {

    if (file.exists(destfile)) {
      cat(
        "\nSkipping", basename(destfile), "(already exists)\n"
      )
      return(NULL)
    }

    timer <- PAutilities::manage_procedure(
      "Start", "\nDownloading", basename(destfile)
    )

    utils::download.file(
      url, destfile, quiet = TRUE, mode = "wb"
    ) %T>%
    {PAutilities::manage_procedure("End", timer = timer)}

  }

  urls <- c(

    ## 2003-2004 (Excluded links have no counterpart from 2005-2006)
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT",
      #"https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BIX_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPX_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT",
      #"https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/CVX_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13AM_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13_C.XPT",
      #"https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/SSFA_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/UC_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/SMQ_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPQ_C.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/MCQ_C.XPT",

    ## 2005-2006
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPX_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/HDL_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/TRIGLY_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/TCHOL_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GHB_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/UCPREG_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/SMQ_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPQ_D.XPT",
      "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/MCQ_D.XPT"

  )

  urls %>%
  basename(.) %>%
  file.path("0_Input", .) %>%
  {mapply(get_xpt, urls, .)} %>%
  {rm(list = ls(envir = globalenv()), envir = globalenv())} %>%
  invisible(.)

# ZIP Files ---------------------------------------------------------------

  get_zip <- function(url, destfile) {

    if (file.exists(
      gsub("zip$", "xpt", destfile, ignore.case = TRUE)
    )) {
      cat(
        "\nSkipping", basename(destfile), "(already exists)\n"
      )
      return(NULL)
    }

    timer <- PAutilities::manage_procedure(
      "Start", "\nDownloading and unzipping", basename(destfile),
      " -- it's a large zip archive so this will take awhile\n"
    )

    destfile %T>%
    utils::download.file(url, ., mode = "wb") %T>%
    utils::unzip(exdir = dirname(destfile)) %T>%
    file.remove(.) %T>%
    {PAutilities::manage_procedure("End", timer = timer)}

  }

  urls <- c(
    "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAXRAW_C.ZIP",
    "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAXRAW_D.ZIP"
  )

  urls %>%
  basename(.) %>%
  file.path("0_Input", .) %>%
  {mapply(get_zip, urls, .)} %>%
  {rm(list = ls(envir = globalenv()), envir = globalenv())} %>%
  invisible(.)

# Tracker -----------------------------------------------------------------

  if (!dir.exists("0_Input/rds")) {
    dir.create("0_Input/rds")
  }

  data.table::fread(
    "0_Input/tracker.csv", stringsAsFactors = FALSE
  ) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  saveRDS("0_Input/rds/tracker.rds")
