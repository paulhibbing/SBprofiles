# Convenience functions ---------------------------------------------------

tertilize <- function(x) {
  
  tertiles <- quantile(x, probs = c(0.33, 0.67))
  
  labs <-
    format(tertiles, digits = 1, nsmall = 1) %>%
    c(., "Inf") %>%
    paste0("T", 1:3, " (", ., ")")
  
  c(-Inf, tertiles, Inf) %>%
  cut(x, ., labs)
  
}

get_OR_table <- function(d, M1, M2, M3, xvar = "forest_profile") {
  wrapper(d, M1, "M1", xvar) %>%
  cbind(wrapper(d, M2, "M2", xvar)) %>%
  cbind(wrapper(d, M3, "M3", xvar)) %>%
  stats::setNames(., gsub("^M[0-9]+_G", "G", names(.))) %>%
  .[ ,!duplicated(names(.))]
}


# Modeling functions ------------------------------------------------------

##CONTENTS:
## * `get_p` to retrieve and format coefficient p-values
## * `full_table` to retrieve and format ORs and p-values in tabular form
## * `fit_model` to fit a logistic model using categorical xvar with an option
##   to fit a trend model or standard
## * `wrapper` to call the previous three functions in such a way as to extract
##   a complete table of information

get_p <- function(model, xvar) {

  stopifnot(inherits(model, "glm"))
  
  summary(model)$coefficients %>%
  .[ ,"Pr(>|z|)"] %>%
  .[grepl(xvar, names(.))] %>%
  round(4) %>%
  format(digits = 4, nsmall = 4, scientific = FALSE) %>%
  {ifelse(grepl("^0.000", .), "<0.001", .)}
  
}

full_table <- function(full, xvar) {
  
  stopifnot(inherits(full, "glm"))
  
  or <-
    full$coefficients %>%
    .[grepl(xvar, names(.))] %>%
    stats::setNames(., gsub(xvar, "", names(.))) %>%
    exp(.) %>%
    format(digits = 2, nsmall = 2)
  
  ci <-
    confint(full) %>%
    exp(.) %>%
    apply(1, format, digits = 2, nsmall = 2) %>%
    apply(2, paste, collapse = ", ") %>%
    .[grepl(xvar, names(.))] %>%
    stats::setNames(., gsub(xvar, "", names(.))) %T>%
    {stopifnot(identical(names(.), names(or)))}
  
  full_tab <-
    paste0(or, " (", ci, ")") %>%
    c("Referent", names(or), "Referent", .) %>%
    matrix(ncol = 2)
  
  get_p(full, xvar) %>%
  c("", .) %>%
  matrix(ncol = 1) %>%
  cbind(full_tab, .) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  stats::setNames(c("Group", "OR_95CI", "P")) %>%
  within({Group = full$xlevels[[xvar]]})
  
}

fit_model <- function(
  d, yvar, xvar, trend = FALSE
) {
  
  stopifnot(is.factor(d[ ,xvar]))
  
  if (trend) {
    d[ ,xvar] %<>% as.numeric(.)
  }
  
  paste0(yvar, " ~ .") %>%
  as.formula(.) %>%
  stats::glm("binomial", d)
  
}

wrapper <- function(
  d, covariates = NULL, label,
  xvar = "forest_profile", yvar = "high_risk"
) {
  
  d %<>%
    .[ ,c(yvar, xvar, covariates)] %T>%
    {stopifnot(all(stats::complete.cases(.)))}

  paste(covariates, collapse = " + ") %>%
  paste(xvar, ., sep = " + ") %>%
  paste(yvar, "~", .) %>%
  gsub(" [+]* *$", "", .) %>%
  paste0(label, ": ", .) %>%
  print(.)
  
  full  <- fit_model(d, yvar, xvar, FALSE)
  trend <- fit_model(d, yvar, xvar, TRUE)
  
  get_p(trend, xvar) %>%
  c("P-trend", ., "") %>%
  matrix(1) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  stats::setNames(c("Group", "OR_95CI", "P")) %>%
  rbind(full_table(full, xvar), .) %>%
  within({Group = paste(" ", Group)}) %>%
  rbind(paste("Exposure:", xvar), .) %>%
  stats::setNames(., paste0(label, "_", names(.)))
  
}
