

#' Update a Shiny input
#'
#' This function updates a Shiny input based on the type of input and new values provided.
#' It uses the `update_info` data frame to find the appropriate update function and its arguments.
#'
#' @param input_id The id of the inputs to be updated.
#' @param input_type The type of the input (e.g., "selectInput", "checkboxInput").
#' @param new_values A named list of new values for the input.
#'  The names should correspond to the arguments of the update function.
#'
#' @return None. This function is used for its side effect of updating a Shiny input.
#' @export
#'
#' @examples
#' \dontrun{
#'  library(shiny)
#'  library(shinyinvoer)
#'
#' ui <- fluidPage(
#'   uiOutput("numeric_out"),
#'   uiOutput("slider_out")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   output$numeric_out <- renderUI({
#'     numberInput("id_test",
#'                 "Number of observations",
#'                 value = 70,
#'                 min = 10,
#'                 max = 100)
#'   })
#'
#'   output$slider_out <- renderUI({
#'     sliderInput("id_slide",
#'                 "Slide value",
#'                 value = 7,
#'                 min = 3,
#'                 max = 15)
#'   })
#'
#'
#'   observe({
#'     if (!is.null(input$id_slide)) {
#'       if (input$id_slide == 10) {
#'         update_input(session, "id_test", "numberInput", new_values = 10)
#'       }
#'     }
#'   })
#'
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' }
update_input <- function(session, input_id, input_type, new_values) {
  if (is.null(input_type)) return()
  update_info <- update_info |> dplyr::filter(input %in% input_type)
  update_func <- getFromNamespace(update_info$update, update_info$library)
  update_arg <- strsplit(update_info$update_arg, split = "\\|") |> unlist()
  new_values <- list(new_values)
  names(new_values) <- update_arg
  update_args <- c(list(session, input_id), new_values)
  do.call(update_func, update_args)
}




