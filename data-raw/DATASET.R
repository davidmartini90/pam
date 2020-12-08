## code to prepare `DATASET` dataset goes here

load("data-raw/example_data.RData")

usethis::use_data(example_data, overwrite = TRUE)
