# Setup -------------------------------------------------------------------

  rm(list = ls())
  library(magrittr)
  library(ggplot2)
  
  d <- readRDS("1_Bout_Data/d5_final.rds")

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
  #   Removing 167 outliers 
  #   (leaves n of 5495)
  
# PCA to look at important variables --------------------------------------

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
  
  ## n = 7 PCs fit the bill. Max variance explained is 97.9%, per below
    # summary(pcs)$importance[ ,seq(n)]
    
  ## Now look at the loadings
    
    # seq(n) %>%
    # lapply(function(x) {
    #   pcs$rotation[ ,x] %>%
    #   abs(.) %>%
    #   .[order(., decreasing = TRUE)]
    # })
    
    ## First component is driven "equally" by everything but 10th percentile and
    ## bout rate
    
    ## Second component is driven by bouts rate
    
    ## Third is driven by 10th percentile
    
    ## Fourth is driven by 10th percentile
    
    ## Fifth is driven by 20th percentile
    
    ## Sixth is driven by 25th percentiles
    
    ## Seventh is driven by 25th and 30th percentiles

# Optimal PCA-based clusters ----------------------------------------------

    pcdata <- pcs$x[ ,1:n]
    
    ## Below code is time consuming to run -- uncomment
    ## if it needs to be redone
    
      # set.seed(610)
      # 
      # pdf("2a_Cluster/1b_Optimal_Clustering.pdf")
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

    ## Silhouette method suggests 2 clusters, elbow method suggests 4, and gap
    ## statistic suggests 5 -- Let's go for 3, since that gives an even
    ## case distribution compared to other k (see below)
    
    # sapply(2:5, function(n, pcdata) {
    #   cluster::pam(pcdata, n, cluster.only = TRUE) %>% table(.)
    # }, pcdata = pcdata, simplify = FALSE)
    
      # [[1]]
      # .
      # 1    2 
      # 3582 1913 
      # 
      # [[2]]
      # .
      # 1    2    3 
      # 1740 2453 1302 
      # 
      # [[3]]
      # .
      # 1    2    3    4 
      # 1606 1259 1829  801 
      # 
      # [[4]]
      # .
      # 1    2    3    4    5 
      # 1606 1259 1510  687  433 

# Look at medoids ---------------------------------------------------------

    set.seed(610)
    
    m <- cluster::pam(pcdata, 3, keep.diss = FALSE, keep.data = FALSE)
    
    m$medoids %>%
    row.names(.) %>%
    d[.,variables] %T>%
    View("medoids") %>%
    data.table::fwrite("2a_Cluster/1c_Medoids.csv")
    
# Assign clusters ---------------------------------------------------------

    set.seed(610)
    
    cluster::pam(pcdata, 3, cluster.only = TRUE) %>%
    unname(.) %>%
    factor(labels = c("Avoider", "Limiter", "Prolonger")) %>%
    {within(d, {cluster = .})} %>%
    saveRDS("2a_Cluster/rds/clustered_d.rds")
