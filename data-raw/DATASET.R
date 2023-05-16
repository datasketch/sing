#This list is a comprehensive mapping of Shiny inputs to their corresponding
#update functions, along with necessary parameters and the libraries they belong to.
library(readr)
update_info <- read_csv("data-raw/data/sing/general-input.csv")
usethis::use_data(update_info, overwrite = TRUE)


input_list <- jsonlite::read_json("data-raw/data/inputs.json")
input_list$selector
names(input_list$inputs)
