#' @export
render_sing <- function(session,
                        input,
                        bd,
                        input_params,
                        sing_values) {

  data_server <- reactiveValues()
  inputs_user <- reactiveValues()
  inputs_data <- reactiveValues()
  id_inputs <- setdiff(names(input_params$inputs), c("exclude", "include"))

  observe({
    if (!is.null(input[["what_table_input"]])) {
      data_server[[input[["what_table_input"]]]] <- bd$hdtables[[input[["what_table_input"]]]]$data
    }

    pre_inp <- prepare_inputs(input = input,
                              inputs_user = inputs_user,
                              inputs_data = inputs_data,
                              input_params = input_params,
                              input_names = id_inputs,
                              sing_values = sing_values,
                              data_server = data_server)


    purrr::map(id_inputs, function(id) {
      if (!is.null(input[[id]])) {
        update_input(session = session,
                     input_id = id,
                     input_type = input_params$inputs[[id]]$input_type,
                     new_values = input[[id]])
      }
    })

  })


  list(
    data_server = data_server,
    inputs_user = inputs_user,
    inputs_id = id_inputs,
    input_params = input_params,
    inputs_data = inputs_data
  )
}


#' @export
output_sing <- function(input, output, id_inputs, input_params, inputs_data) {
  purrr::map(id_inputs, function(id){
    output[[id]] <- renderUI({
      pass_condition <- TRUE
      if ("conditions" %in% names(input_params$inputs[[id]])) {
        input_conditions <- input_params$inputs[[id]]$conditions
        input_depend <- names(input_conditions)
        if (!is.null(input[[input_depend]])) {
          pass_condition <- sing:::primary_conditions(input, input_id = input_depend,
                                                      condition = names(input_conditions[[input_depend]]),
                                                      comparison_value = input_conditions[[input_depend]][[names(input_conditions[[input_depend]])]])

        }
      }
      if (pass_condition) {
        do.call(inputs_data[[id]]$input_type, inputs_data[[id]]$input_params)
      }
    })
  })
}


#' @export
sing <- function(session, input, output, output_id, results) {
  inputs_data <- results$inputs_data
  output_sing(input,
              output,
              results$inputs_id,
              input_params = input_params,
              inputs_data = inputs_data)

  output[[output_id]] <- renderUI({
    purrr::map(results$inputs_id, ~{
      uiOutput(.x)
    })
  })
}
