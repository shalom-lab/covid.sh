## code to prepare `my_dataset` dataset goes here
load('data-raw/data.rda')

usethis::use_data(case.asym,overwrite = TRUE)
usethis::use_data(map.2.new,overwrite = TRUE)
usethis::use_data(shanghai,overwrite = TRUE)

