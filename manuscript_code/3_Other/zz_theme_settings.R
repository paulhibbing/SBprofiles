library(ggplot2)
theme_settings <-
  theme_classic() +
  theme(
    axis.line = element_line(size = .5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold")
  )
