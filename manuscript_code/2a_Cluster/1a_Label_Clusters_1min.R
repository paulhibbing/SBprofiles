# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  library(ggplot2)

  rstudioapi::getActiveDocumentContext()$path %>%
  dirname(.) %>%
  dirname(.) %>%
  setwd(.)

  d <- readRDS("1_Bout_Data/d1_final.rds")

  source("2a_Cluster/zz_features.R")

# Outlier screening -------------------------------------------------------

  outlier <- function(x) {
    sd(x) %>%
    {. * c(-3,3)} %>%
    {mean(x) + .} %>%
    {data.table::inrange(x, .[1], .[2])} %>%
    {!.}
  }

  d <-
    d[ ,variables] %>%
    lapply(outlier) %>%
    do.call(cbind, .) %>%
    apply(1, any) %T>%
    {if (sum(.) > 0) warning(
        "Removing ", sum(.),
        " outliers", call. = FALSE
    )} %>%
    {d[!., ]} %>%
    structure(., row.names = seq(nrow(.)))

  # Warning message:
  #   Removing 347 outliers
  #   (leaves n of 5315)

# PCA to look at important variables --------------------------------------

  variables <-
    sapply(d[ ,variables], sd) %>%
    .[.==0] %>%
    {if(!!length(.)) {
      names(.) %>%
      paste(collapse = " -- ") %>%
      message("Removing non-varying ", .)
      setdiff(variables, names(.))
    } else {variables}}

  pcs <-
    d[ ,variables] %>%
    stats::prcomp(center = TRUE, scale = TRUE)

  ## Focus on cumulative variance explained, and include all PCs that yield at
  ## least 1% increase

    n <-
      summary(pcs)$importance %>%
      .["Cumulative Proportion", ] %>%
      diff(.) %>%
      .[. > 0.01] %>%
      names(.) %>%
      .[length(.)] %>%
      gsub("^PC", "", .) %>%
      as.numeric(.)

  ## n = 7 PCs fit the bill. Max variance explained is 98.0%, per below
  ## individually 1.6% to 70.6%
    # summary(pcs)$importance[ ,seq(n)]

  ## Now look at the loadings

    # seq(n) %>%
    # lapply(function(x) {
    #   pcs$rotation[ ,x] %>%
    #   abs(.) %>%
    #   .[order(., decreasing = TRUE)]
    # })

    ## First component is driven by high percentiles and ranges

    ## Second component is driven by 25th percentile

    ## Third is driven by bout frequency and 25th percentile

    ## Fourth is driven by 30th percentile

    ## Fifth is driven by 40th percentile

    ## Sixth is driven by median and 30th percentile

    ## Seventh is driven by median and SB%

# Optimal PCA-based clusters ----------------------------------------------

    pcdata <- pcs$x[ ,1:n]

    ## Below code is time consuming to run -- uncomment
    ## if it needs to be redone
    ## (RStudio for Windows: highlight and press ctrl + shift + C)

      # set.seed(610)
      #
      # pdf("2a_Cluster/1b_Optimal_Clustering_1min.pdf")
      #
      #   factoextra::fviz_nbclust(
      #     pcdata, cluster::pam, method = "wss",
      #     keep.diss = FALSE, keep.data = FALSE
      #   ) +
      #   ggtitle("Optimal Clusters (Elbow Method)")
      #
      #   factoextra::fviz_nbclust(
      #     pcdata, cluster::pam, method = "silhouette",
      #     keep.diss = FALSE, keep.data = FALSE
      #   ) +
      #   ggtitle("Optimal Clusters (Silhouette Method)")
      #
      #   cluster::clusGap(
      #     pcdata, cluster::pam, 10, B = 10,
      #     keep.diss = FALSE, keep.data = FALSE
      #   ) %>%
      #   factoextra::fviz_gap_stat(.) +
      #   ggtitle("Optimal Clusters (Gap Statistic)")
      #
      # dev.off()

    ## Silhouette method suggests 2 clusters, elbow method suggests 5, and gap
    ## statistic suggests 7 -- Let's go for 3, since that gives an even
    ## case distribution compared to other k (see below)

    # sapply(2:7, function(n, pcdata) {
    #   cluster::pam(pcdata, n, cluster.only = TRUE) %>% table(.)
    # }, pcdata = pcdata, simplify = FALSE)

      # [[1]]
      # .
      # 1    2
      # 3445 1870
      #
      # [[2]]
      # .
      # 1    2    3
      # 2381 1562 1372
      #
      # [[3]]
      # .
      # 1    2    3    4
      # 1945  828 1353 1189
      #
      # [[4]]
      # .
      # 1    2    3    4    5
      # 1932  398 1353 1122  510
      #
      # [[5]]
      # .
      # 1    2    3    4    5    6
      # 1175  397 1010 1340  899  494
      #
      # [[6]]
      # .
      # 1    2    3    4    5    6    7
      # 1129  492  954 1338  608  468  326

# Look at medoids ---------------------------------------------------------

    set.seed(610)

    m <- cluster::pam(pcdata, 3, keep.diss = FALSE, keep.data = FALSE)

    m$medoids %>%
    row.names(.) %>%
    d[.,variables] %T>%
    View("medoids") %>%
    data.table::fwrite("2a_Cluster/1c_Medoids_1min.csv")

# Assign clusters ---------------------------------------------------------

    set.seed(610)

    cluster::pam(pcdata, 3, cluster.only = TRUE) %>%
    unname(.) %>%
    factor(c(3, 1, 2), c("Interrupted", "Intermediate", "Prolonged")) %>%
    {within(d, {cluster = .})} %>%
    saveRDS("2a_Cluster/rds/clustered_d_1min.rds")
