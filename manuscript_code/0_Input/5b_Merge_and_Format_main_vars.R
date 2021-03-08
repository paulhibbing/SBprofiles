# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  vars <- c(
    "SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDEXPRG",
    paste0("BPXSY", 1:4), paste0("BPXDI", 1:4),
    "BMXBMI", "LBXTR", "LBDTRSI", "LBDLDL", "LBDLDLSI",
    "LBXHDD", "LBDHDDSI", "LBXTC", "LBDTCSI", "LBXGH", "URXPREG",
    "DIQ010", "SMQ020", "SMQ040", "BPQ020", "BPQ040A", "BPQ050A",
    paste0("MCQ160", LETTERS[2:6])
  )

  labels <- c(
    "id", "sex", "age", "race", "exam_pregnancy",
    paste0("sbp", 1:4), paste0("dbp", 1:4),
    "bmi", "triglycerides", "triglycerides_SI",
    "ldl", "ldl_SI", "hdl", "hdl_SI", "cholesterol",
    "cholesterol_SI", "glycohemoglobin", "pregnancy",
    "diabetes", "SMQ020", "smoking", "BPQ020", "BPQ040A",
    "bp_prescription", "chf", "chd", "angina", "mi", "stroke"
  )

  # View(data.frame(vars, labels))

# Set up one heavy lifting function for formatting the data later on ------

  format_variable <- function(
    vars, varname, keep = c(2, NA), na_val = 1,
    labels = list(NA, FALSE)
    #Note: The order of `labels` depends on `order(c(keep, na_val))` because
    #`switch()` will correctly interpret vars[ ,varname] as integer, not factor
  ) {

    stopifnot(
      varname %in% names(vars),
      length(na_val) == 1,
      is.integer(vars[ ,varname]),
      is.list(labels)
    )

    if (NA %in% keep) {

      stopifnot(
        length(na.omit(keep)) > 0,
        length(labels) == sum(length(keep), length(na_val)) - 1
      )

      message("Setting NA to ", na.omit(keep)[1], " (", varname, ")")

      vars[ ,varname] %<>%
        is.na(.) %>%
        ifelse(na.omit(keep)[1], vars[ ,varname])

    } else {

      stopifnot(length(labels) == sum(length(keep), length(na_val)))

    }

    vars[ ,varname] %<>%
      {. %in% keep} %>%
      ifelse(vars[ ,varname], na_val) %>%
      list(EXPR = .) %>%
      c(labels) %>%
      {mapply(switch, .[[1]], MoreArgs = .[-1])}

    vars

  }

# Read and merge DVs ------------------------------------------------------

  vars <-
    list.files(
      "0_input/rds",
      "rds$",
      full.names = TRUE,
      include.dirs = FALSE
    ) %>%
    {.[!grepl("tracker", .)]} %>%
    {.[!grepl("vars.rds$", .)]} %>%
    {.[!grepl("accelerometer_valid.rds$", .)]} %>%
    lapply(
      function(x, vars) {
        readRDS(x) %>%
        .[ ,intersect(names(.), vars)]
      },
      vars
    ) %>%
    Reduce(
      function(x,y) merge(x,y,all=TRUE), .
    ) %>%
    .[ ,vars] %>%
    stats::setNames(labels) %>%
    {.[ ,!grepl("_SI$", names(.))]} %T>%
    {rm(labels, envir = globalenv())}

# Label valid accelerometer files -----------------------------------------

  # See opening note in `0_Input/5a_accel_file_labels.R` for an explanation of
  # why this is set up this way (It's to avoid re-running a very time-consuming
  # bit of code over and over)
  vars <-
    readRDS("0_Input/rds/accelerometer_valid.rds") %>%
    merge(vars, ., all = TRUE) %T>%
    {stopifnot(nrow(.) == nrow(vars))}

# Pre-format raw data set (for documentation of missing data) -------------

  ## First remove attributes

    for (col in names(vars)) {
      attributes(vars[ ,col]) <- NULL
    }

    rm(col)

  ## Address factors (why not?)

    vars$sex %<>% factor(
      labels = c("Male", "Female")
    )

    vars$race %<>% factor(
      labels = c(
        "Mexican American", "Other Hispanic",
        "Non-Hispanic White", "Non-Hispanic Black",
        "Other"
      )
    )

  ## Account for repeat measures of blood pressure

    vars$sbp1 <-
      vars[ ,paste0("sbp", 1:4)] %>%
      apply(1, mean, na.rm = TRUE) %>%
      {ifelse(is.nan(.), NA, .)} %>%
      {2 * round(./2)}

    vars$dbp1 <-
      vars[ ,paste0("dbp", 1:4)] %>%
      apply(1, mean, na.rm = TRUE) %>%
      {ifelse(is.nan(.), NA, .)} %>%
      {2 * round(./2)}

    vars %<>%
      names(.) %>%
      gsub("bp1$", "bp", .) %>%
      stats::setNames(vars, .) %>%
      {.[ ,!grepl("^[ds]bp[234]{1}$", names(.))]}

  ## Account for smoking skip pattern

    vars %<>%
      format_variable("SMQ020", 1:2, 3, list(TRUE, FALSE, NA)) %>%
      format_variable("smoking", 1:3, 4, list(TRUE, TRUE, FALSE, NA))

    vars <-
      sapply(!vars$SMQ020, isTRUE) %T>% ## (< 100 lifetime cigarettes)
      {stopifnot(all(is.na(vars$smoking[.])))} %>% ## Thus didn't answer 040
      ifelse(FALSE, vars$smoking) %>% ## So 040 should be FALSE in those cases
      {within(vars, {
        smoking = .
        SMQ020 = NULL
      })}

  ## Account for blood pressure medication skip pattern

    vars %<>%
      format_variable("BPQ020", 1:2, 3, list(TRUE, FALSE, NA)) %>%
      format_variable("BPQ040A", 1:2, 3, list(TRUE, FALSE, NA)) %>%
      format_variable("bp_prescription", 1:2, 3, list(TRUE, FALSE, NA))

    vars <-
      sapply(!vars$BPQ020, isTRUE) %>%
      ## ^^ (Never told high BP)
      {. | sapply(!vars$BPQ040A, isTRUE)} %T>%
      ## ^^ (OR medication never prescribed)
      {stopifnot(all(is.na(vars$bp_prescription[.])))} %>%
      ## ^^ Thus didn't answer 050A
      ifelse(FALSE, vars$bp_prescription) %>%
      ## ^^ So 050A is FALSE in those cases
      {within(vars, {
        bp_prescription = .
        BPQ020 = NULL
        BPQ040A = NULL
      })}

  ## Save the raw copy before proceeding (missing data documentation will follow
  ## in `0_Input/6b_Document_Missing_Data.R`)

    saveRDS(vars, "0_Input/rds/raw_vars.rds")

# Formal formatting -------------------------------------------------------

  ## No further action needed for id, age, bmi, TG,
  ## LDL, HDL, cholesterol, or gHb

  ## Logicals (positives are coded as NA, because we will use
  ## stats::complete.cases to remove missing and positive all at once)

    vars %<>%
      within({pregnancy = pregnancy + 1L}) %T>%
      # ^^ `switch()` won't accept values of 0
      {stopifnot(all(na.omit(.$pregnancy) > 0L))} %>%
      format_variable(
        "pregnancy", c(1:2, 4:5), 3, list(NA, NA, FALSE, NA, NA)
      ) %>%
      format_variable("exam_pregnancy", c(1,3), 2, list(NA, FALSE, NA)) %>%
      format_variable("chf") %>%
      format_variable("chd") %>%
      format_variable("angina") %>%
      format_variable("mi") %>%
      format_variable("stroke") %>%
      format_variable("diabetes", 1:3, 4, list(TRUE, FALSE, TRUE, NA))

  ## De novo calculations: Weight status only (have to wait to calculate CVD
  ## risk until after missing data removal, since currently the function does
  ## not accommodate missing values)

    new_names <-
      names(vars) %>%
      append(., "weight_status", which(. == "bmi"))

    vars %<>%
      within({
        weight_status = sapply(
          bmi, PAutilities::weight_status
        )
      }) %>%
      .[ ,new_names] %T>%
      {rm(new_names, envir = globalenv())}

# Remove cases with incalculable risk (incomplete data) -------------------

  required_vars <- c(
    "age", "sex", "cholesterol", "hdl",
    "sbp", "bp_prescription",
    "diabetes", "smoking", "accel_file",
    "exam_pregnancy", "pregnancy",
    "chf", "chd", "angina", "mi", "stroke"
  )

  drop_vars <- c(
    "exam_pregnancy", "pregnancy", "chf",
    "chd", "angina", "mi", "stroke"
  )

  vars %<>%
    .[ ,required_vars] %>%
    stats::complete.cases(.) %>%
    vars[., ] %T>%
    {stopifnot(sum(sapply(.[ ,drop_vars], sum)) == 0)} %>%
    .[ ,setdiff(names(vars), drop_vars)] %>%
    structure(., row.names = seq(nrow(.)))

  ## Check on the major variables
    # sapply(vars, summary, simplify = FALSE) %>%
    # .[intersect(names(.), required_vars)]

  ## Check the other variables
    # sapply(vars, summary, simplify = FALSE) %>%
    # .[setdiff(names(.), required_vars)]

# Calculate risk and save the dataset -------------------------------------

  vars %>%
  PAutilities::cvd_risk(
    sex = "sex", age = "age", total_cholesterol = "cholesterol",
    hdl = "hdl", systolic = "sbp", bp_treated = "bp_prescription",
    diabetes = "diabetes", smoker = "smoking", points = FALSE
  ) %>%
  {within(vars, {cvd_risk = .})} %>%
  saveRDS("0_Input/rds/vars.rds")
