rm(list = ls())
devtools::load_all()
library(magrittr)

load("data/example_data.rda")

# object <- example_data
# method = "both"
# id = NULL
# counts = "PAXINTEN"
# wear = NULL
# sb = 100
# valid_indices = NULL

get_profile(example_data, counts = "PAXINTEN", method = "decisionTree")
