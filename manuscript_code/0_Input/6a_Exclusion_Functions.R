## These functions are designed for documenting data loss, but may also come in
## handy for reducing datasets for analysis

print_table <- function(x) {
  
  table(x) %>%
  {mapply(paste, ., names(.), sep = " for ", SIMPLIFY = FALSE)} %>%
  paste(collapse = ", ") %>%
  paste0("\n  (", ., ")")
  
}

exclude_accel_exists <- function(vars, ...) {
  
  vars %T>%
  {message(
    "Removing ", sum(is.na(.$accel_file)),
    " of ", nrow(.), " participant(s) who had no",
    " monitor data (remaining n = ", sum(!is.na(.$accel_file)), ")"
  )} %>%
  .[!is.na(.$accel_file), ]
    
}

exclude_age <- function(vars, age, ...) {
  
  vars %T>%
  {message(
    "Removing ", sum(.$age < age),
    " participant(s) younger than ", age,
    " years old (remaining n = ", sum(.$age >= age), ")"
  )} %>%
  .[.$age >= age, ]
    
}

exclude_pregnancy <- function(vars, ...) {
  
  vars %T>%
  {message(
    "Removing ",
    sum(.$exam_pregnancy %in% c(1,3) | .$pregnancy %in% c(0:1, 3:4)),
    " pregnant women (remaining n = ",
    sum(!(.$exam_pregnancy %in% c(1,3) | .$pregnancy %in% c(0:1, 3:4))),
    ")"
  )} %>%
  {.[!(.$exam_pregnancy %in% c(1,3) | .$pregnancy %in% c(0:1, 3:4)), ]}
  
}

exclude_cvd <- function(vars, varname, ...) {
  
  vars %T>%
  {message(
    "Removing ", sum(.[ ,varname] %in% c(1, 7, 9)),
    " participant(s) with ", varname, " (remaining n = ",
    sum(!.[ ,varname] %in% c(1, 7, 9)), ")"
  )} %>%
  .[!.[ ,varname] %in% c(1, 7, 9), ]
  
}

exclude_smoking <- function(vars, ...) {
  
  vars %T>%
  {message(
    "Removing ", sum(is.na(.$smoking)),
    " participant(s) with unknown smoking status (remaining ",
    "n = ", sum(!is.na(.$smoking)), ")"
  )} %>%
  .[!is.na(.$smoking), ]
    
}

exclude_cholesterol <- function(vars, ...) {

  vars %T>%
    
  {message(
    "Removing ", sum(is.na(.$hdl)),
    " participant(s) with unknown HDL (remaining n = ",
    sum(!is.na(.$hdl)), ")"
  )} %>%
  .[!is.na(.$hdl), ] %T>%
    
  {message(
    "Removing ", sum(is.na(.$cholesterol)),
    " participant(s) with unknown total cholesterol (remaining n = ",
    sum(!is.na(.$cholesterol)), ")"
  )} %>%
  .[!is.na(.$cholesterol), ]
    
}

exclude_bp <- function(vars, ...) {
  
  vars %T>%
  {message(
    "Removing ", sum(is.na(.$sbp)),
    " participant(s) with unknown SBP (remaining n = ",
    sum(!is.na(.$sbp)), ")"
  )} %>%
  .[!is.na(.$sbp), ]
  
}

exclude_antihypertensive <- function(vars, ...) {

  vars %T>%
  {message(
    "Removing ", sum(is.na(.$bp_prescription)),
    " participant(s) with unknown hypertension medication status ",
    "(remaining n = ", sum(!is.na(.$bp_prescription)), ")"
  )} %>%
  .[!is.na(.$bp_prescription), ]
  
}

exclude_diabetes <- function(vars, ...) {
  
  vars %T>%
  {message(
    "Removing ", sum(!.$diabetes %in% 1:3),
    " participant(s) with unknown diabetes status (remaining ",
    "n = ", sum(.$diabetes %in% 1:3), ")"
  )} %>%
  .[.$diabetes %in% 1:3, ]
  
}

exclude_accel_valid <- function(vars, ...) {
  
  vars %T>%
    {message(
      "Removing ", sum(!.$accel_valid),
      " invalid activity monitor files (remaining n = ",
      sum(.$accel_valid), ")",
      print_table(.$accel_status[!.$accel_valid])
    )} %>%
    {.[.$accel_valid, ]}
  
}

get_args <- function(criterion, ...) {
  c("chf", "chd", "angina", "mi", "stroke") %>%
  {ifelse(criterion %in% ., "cvd", criterion)} %>%
  paste0("exclude_", .) %>%
  {eval(parse(text = .))} %>%
  list(fun = ., varname = criterion, ...)
}

load_and_reduce <- function(
  filename = "0_Input/rds/raw_vars.rds",
  criteria = c(
    "accel_exists", "age", "pregnancy",
    "chf", "chd", "angina", "mi", "stroke",
    "smoking", "cholesterol", "bp", "antihypertensive",
    "diabetes", "accel_valid"
  ), age
) {
  
  if (missing(age)) age <- 0
  
  vars <- readRDS(filename)
  funs <- lapply(criteria, get_args, age = age)

  for (fun in funs) {
    fun$vars <- vars
    vars <- do.call(fun[[1]], fun[-1])
  }
  
  vars
  
}
