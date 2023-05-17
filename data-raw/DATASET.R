library(shinyinvoer)
library(shinyWidgets)
library(readr)


#This list is a comprehensive mapping of Shiny inputs to their corresponding
#update functions, along with necessary parameters and the libraries they belong to.

update_info <- read_csv("data-raw/data/sing/general-input.csv")
usethis::use_data(update_info, overwrite = TRUE)

# this list is a mapping of arguments to available inputs
extract_input_arg <- purrr::map(update_info$input, ~ {
  l <- as.list(formals(.x))
  ind_dot <- grep("\\.\\.\\.", names(l))
  if (!identical(ind_dot, integer())) {
    l <- l[-ind_dot]
  }
  l
})
names(extract_input_arg) <-  update_info$input
extract_input_arg$selectizeInput <- modifyList(extract_input_arg$selectizeInput,
                                               extract_input_arg$selectInput, keep.null = TRUE)


input_properties <- list(
  "inputs" =  extract_input_arg
)
usethis::use_data(input_properties, overwrite = TRUE)

