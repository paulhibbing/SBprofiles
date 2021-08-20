# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  d <- readRDS("2a_Cluster/rds/clustered_d_1s.rds")

  source("2a_Cluster/zz_summary_confusionMatrix.R")
  library(ggplot2)

  features <-
    c(
      "SB_perc", "bouts_weartime",
      "Q10_bout", "Q25_bout", "Q50_bout",
      "Q75_bout", "Q90_bout", "cluster"
    ) %T>%
    {stopifnot(all(. %in% names(d)))}

# Fit classification tree and random forest (uncomment to re-run) ---------

  ## Tree

    # tree <-
    #   d[ ,features] %T>%
    #   {set.seed(610)} %>%
    #   tree::tree(cluster~., .) %T>%
    #   saveRDS("2a_Cluster/rds/tree_1s.rds")
    #
    # tiff(
    #   "zz_figures/3a_tree_1s.tif", 7, 7, "in",
    #   res = 1200, compression = "lzw"
    # )
    #   plot(tree)
    #   text(tree, pretty = 0, cex = 0.75)
    # dev.off()

  ## Forest

    # forest <-
    #   d[ ,features]  %T>%
    #   {set.seed(610)} %>%
    #   randomForest::randomForest(
    #     cluster~., .
    #   ) %T>%
    #   saveRDS("2a_Cluster/rds/forest_1s.rds")

# 50-fold cross-validation ------------------------------------------------

  ## Randomly assign folds

    d %<>%
      nrow(.) %T>%
      {set.seed(610)} %>%
      {. / 50} %>%
      ceiling(.) %>%
      rep(seq(50), each = .) %>%
      sample(nrow(d)) %>%
      {within(d, {fold = .})}

  ## Establish fold CV functions

    tree_fold <- function(test, train, features) {

      train[ ,features] %>%
      tree::tree(cluster~., .) %>%
      predict(test, type = "class") %>%
      {within(test, {tree_prediction = .})}

    }

    forest_fold <- function(test, train, features) {

      train[ ,features] %>%
      randomForest::randomForest(
        cluster~., .
      ) %>%
      predict(test, type = "class") %>%
      {within(test, {forest_prediction = .})}

    }

    get_folds <- function(fold, d, features) {

      cat("\rFold", fold, "of", max(d$fold))

      in_fold <- d$fold == fold
      test <- d[in_fold, ]
      train <-d[!in_fold, ]

      tree_fold(test, train, features) %>%
      forest_fold(train, features)

    }

  ## Run the CVs

    d %<>%
      {unique(.$fold)} %>%
      .[order(.)] %>%
      lapply(get_folds, d, features) %>%
      do.call(rbind, .)

  ## Results

    tree_results <-
      d %$%
      caret::confusionMatrix(tree_prediction, cluster) %>%
      summary(.) %>%
      data.frame(Model = "Tree", ., stringsAsFactors = FALSE)

    forest_results <-
      d %$%
      caret::confusionMatrix(forest_prediction, cluster) %>%
      summary(.) %>%
      data.frame(Model = "Forest", ., stringsAsFactors = FALSE)

    rbind(tree_results, forest_results) %>%
    data.table::fwrite("2a_Cluster/3b_CV_Results_1s.csv")
