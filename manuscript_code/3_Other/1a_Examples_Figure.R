# Set up and define data --------------------------------------------------

  rm(list = ls())
  library(magrittr)
  source("zz_theme_settings.R")
  
  set.seed(610)
  
  d <- data.frame(
    person = LETTERS[1:4],
    duration = c(100, 100, 100, 50)
  )

  get_segments <- function(bars) {
    geom_segment(
      aes(x = x, xend = xend, y = y, yend = yend),
      data = bars
    )
  }
  
# Define bars -------------------------------------------------------------

  a_bars <-
    data.frame(
      x = 0.55, xend = 1.45,
      y = 50, yend = 50
    ) %>%
    get_segments(.)
  
  b_bars <-
    seq(20, 80, 20) %>%
    data.frame(
      x = 1.55, xend = 2.45,
      y = ., yend = .
    ) %>%
    get_segments(.)
  

  v1 <-
    seq(8, 44, 5) %>%
    sample(3)
  
  c_bars <-
    data.frame(
      x = 2.55, xend = 3.45,
      y = c(v1, v1 + 50, 50),
      yend = c(v1, v1 + 50, 50)
    ) %>%
    get_segments(.)
  
  d_bars <-
    data.frame(
      x = 3.55, xend = 4.45,
      y = v1, yend = v1
    ) %>%
    get_segments(.)

# Create the figure -------------------------------------------------------
  
  "zz_figures/1b_Examples.tif" %>%
  tiff(
    7, 5, "in",
    res = 1200, compression = "lzw"
  )
  
    ggplot(d, aes(person, duration)) +
      geom_bar(
        stat = "identity",
        colour = "black",
        fill = "gray80",
        alpha = 0.5
      ) +
      theme_settings +
      theme(axis.text.x = element_text(
        size = 16, face = "bold"
      )) +
      scale_x_discrete("") +
      scale_y_continuous("SB (min)") +
      a_bars + b_bars + c_bars + d_bars
    
  dev.off()
  