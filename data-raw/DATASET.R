## code to prepare `DATASET` dataset goes here

#usethis::use_data(DATASET, overwrite = TRUE)


input_list <- jsonlite::read_json("data-raw/data/inputs.json")
input_list$selector
names(input_list$inputs)
