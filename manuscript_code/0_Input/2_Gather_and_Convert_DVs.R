## Setup ####

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  tracker <- readRDS("0_Input/rds/tracker.rds")

  outdir <- "0_Input/rds"

## Demographic ####

  c <- SASxport::read.xport(tracker$C[1])
  d <- SASxport::read.xport(tracker$D[1])

    ## certain codes for education, family size,
    ## and race/ethnicity will be lost
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## DMDEDUC DMDFMSIZ RIDRETH2
      ##       1        1        1

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "demographic.rds"))

  rm(c,d)

## Blood Pressure ####

  c <- SASxport::read.xport(tracker$C[2])
  d <- SASxport::read.xport(tracker$D[2])

    ## Variables will be lost from `c` for what
    ## was reported to the participant
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## BPXDAR BPXSAR
      ##      1      1

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "blood_pressure.rds"))

  rm(c,d)

## Body measures ####

  c <- SASxport::read.xport(tracker$C[3])
  d <- SASxport::read.xport(tracker$D[3])

    ## Some data screening variables will be
    ## lost from `c`
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## BMDARMLF BMDCALFF  BMDLEGF BMDRECUF  BMDSUBF BMDTHICF
      ##        1        1        1        1        1        1

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "body_measures.rds"))

  rm(c,d)

## Glycohemoglobin ####

  c <- SASxport::read.xport(tracker$C[4])
  d <- SASxport::read.xport(tracker$D[4])

    ## Data are complete
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## named integer(0)

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "glycohemoglobin.rds"))

  rm(c,d)

## LDL/Triglycerides ####

  c <- SASxport::read.xport(tracker$C[5])
  d <- SASxport::read.xport(tracker$D[5])

    ## Lipoprotein values will be lost from `d`
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## LBDAPBSI   LBXAPB
      ##        1        1

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "ldl_triglycerides.rds"))

  rm(c,d)

## Pregnancy ####

  c <- SASxport::read.xport(tracker$C[6])
  d <- SASxport::read.xport(tracker$D[6])

    ## Data are complete
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## named integer(0)

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "pregnancy.rds"))

  rm(c,d)

## Total Cholesterol/HDL ####

  c <- SASxport::read.xport(tracker$C[7])
  d <-
    tracker$D[7] %>%
    strsplit(", ") %>%
    unlist(.) %>%
    lapply(SASxport::read.xport) %>%
    do.call(merge, .) %>%
    stats::setNames(
      ., gsub("^LBDHDD$", "LBXHDD", names(.))
    )

    ## Data are complete
      # names(c) %>%
      # c(names(d)) %>%
      # table(.) %>%
      # .[.!=2]
      ## named integer(0)

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "cholesterol_hdl.rds"))

  rm(c,d)

## Diabetes ####

  c <- SASxport::read.xport(tracker$C[9])
  d <- SASxport::read.xport(tracker$D[9])

  ## There are pretty substantial differences between variables in 2003-2004
  ## versus 2005-2006, but the key diabetes items match, and that's what matters
    # names(c) %>%
    # c(names(d)) %>%
    # table(.) %>%
    # .[.!=2]


  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "diabetes.rds"))

  rm(c,d)

## Smoking ####

  c <- SASxport::read.xport(tracker$C[10])
  d <- SASxport::read.xport(tracker$D[10])

  ## There are pretty substantial differences between variables in 2003-2004
  ## versus 2005-2006, but the key smoking items match, and that's what matters
    # names(c) %>%
    # c(names(d)) %>%
    # table(.) %>%
    # .[.!=2]


  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "smoking.rds"))

  rm(c,d)

## Hypertension ####

  c <- SASxport::read.xport(tracker$C[11])
  d <- SASxport::read.xport(tracker$D[11])

  ## There are pretty substantial differences between variables in 2003-2004
  ## versus 2005-2006 -- Item 040 had more screening sub-items in the former
  ## cycle than the latter. Only 040A will be used, since that's all that
  ## overlapped between the two cycles
    # names(c) %>%
    # c(names(d)) %>%
    # table(.) %>%
    # .[.!=2]

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "hypertension.rds"))

  rm(c,d)

## Medical ####

  c <- SASxport::read.xport(tracker$C[12])
  d <- SASxport::read.xport(tracker$D[12])

  ## There are pretty substantial differences between variables in 2003-2004
  ## versus 2005-2006, but the CVD items match, and that's what matters
    # names(c) %>%
    # c(names(d)) %>%
    # table(.) %>%
    # .[.!=2]

  intersect(names(c), names(d)) %>%
  {rbind(c[ ,.], d[ ,.])} %>%
  saveRDS(file.path(outdir, "medical.rds"))

  rm(c,d)

## Final check ####

  file.exists(tracker$merged[-8]) %>%
  {stopifnot(all(.))}
