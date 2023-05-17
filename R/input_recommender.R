
#' Shiny Input Recommender
#'
#' This function takes a data frame and optionally a dictionary, and recommends Shiny input controls based on
#' the properties of the columns in the data frame. The recommendation is based on a pre-defined set of rules.
#'
#' @param data A data frame for which to recommend input controls.
#' @param dic An optional dictionary that provides information about the data frame. If not provided, the function
#'   will generate one using `hdtable::create_dic()`.
#'
#' @return This function returns a list of recommended Shiny input controls. Each element of the list is a list
#'   itself that contains the properties for the corresponding input control.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a data frame
#' df <- data.frame(
#'   a = 1:10,
#'   b = letters[1:10],
#'   c = as.Date("2023-01-01") + 1:10
#' )
#' # Use the function to get recommended input controls
#' recommended_inputs <- input_recommender(df)
#' }
input_recommender <- function(data, dic = NULL, inputs_from_json = NULL) {
  if (is.null(data)) return()
  if (is.null(dic)) {
    dic <- hdtable::create_dic(data)
  }
  if (!"input" %in% names(dic)) {
    default_input <- update_info[,c("hdtype", "input")][!is.na(update_info$default),]
    dic <- dic |> dplyr::left_join(default_input, by = "hdtype")
  }

  input_recommender <- list(inputs = NULL)

  input_recommender <- purrr::map(dic$id, ~ {
    info_id <- dic |>
      dplyr::filter(id %in% .x)
    input_dic <- info_id$input
    input_recommender$inputs[[.x]] <- input_properties$inputs[input_dic]
    input_recommender$inputs[[.x]][[input_dic]]$inputId <- paste0(.x, "_input")
    input_recommender$inputs[[.x]][[input_dic]]$label <- info_id$label


    if ("choices" %in% names(input_recommender$inputs[[.x]][[info_id$input]])) {
      input_recommender$inputs[[.x]][[input_dic]]$choices <- setNames(dic$format[[.x]]$categories, dic$format[[.x]]$labels)
    }


    if (input_dic == "dateRangeInput") {
      input_recommender$inputs[[.x]]$dateRangeInput$start <- info_id$stats[[.x]]$min
      input_recommender$inputs[[.x]]$dateRangeInput$end <- info_id$stats[[.x]]$max
      input_recommender$inputs[[.x]]$dateRangeInput$min <- info_id$stats[[.x]]$min
      input_recommender$inputs[[.x]]$dateRangeInput$max <- info_id$stats[[.x]]$max
    }

    if (input_dic %in% c("sliderInput", "numberInput")) {
      input_recommender$inputs[[.x]][[input_dic]]$value <- info_id$stats[[.x]]$min
      input_recommender$inputs[[.x]][[input_dic]]$min <- info_id$stats[[.x]]$min
      input_recommender$inputs[[.x]][[input_dic]]$max <- info_id$stats[[.x]]$max
    }

    input_recommender
  })

  input_recommender
}
