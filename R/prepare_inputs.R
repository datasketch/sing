#' Prepare Inputs
#'
#' This function is used to prepare the inputs by calling the internal_conditions function
#' and storing in a reactive values, the name of the input and its arguments.
#'
#' @param input A list of inputs.
#' @param inputs_user A list of user inputs.
#' @param inputs_data A list of data inputs.
#' @param sing_values The reactive values.
#' @param input_params Parameters of the input.
#' @param input_names The names of the inputs.
#' @param data_server The data server.
#' @export
prepare_inputs <- function(input,
                           inputs_data,
                           sing_values = NULL,
                           input_params,
                           input_names,
                           data_server = NULL) {

  purrr::map(input_names, function(id){
    inputs_data[[id]]$input_type <- input_params$inputs[[id]]$input_type
    input_arg <- input_params$inputs[[id]]$input_params
    input_names_arg <- names(input_arg)

    input_arg <-  purrr::map(input_names_arg, function(arg) {
      conf_list <- input_arg[[arg]]
      in_id <- input_params$inputs[[id]]$id
      if ("id_alt" %in% names(input_params$inputs[[id]])) {
        in_id <- c(input_params$inputs[[id]]$id, input_params$inputs[[id]]$id_alt)
      }
      conf_list <- internal_conditions(input = input,
                                       conf_list = conf_list,
                                       sing_values = sing_values,
                                       input_id = in_id,
                                       input_params = input_params,
                                       data_server = data_server)
      conf_list
    })
    names(input_arg) <- input_names_arg
    inputs_data[[id]]$input_params <- c(list(inputId = id), input_arg)
  })
}
