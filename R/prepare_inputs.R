#' @export
prepare_inputs <- function(input,
                           inputs_user,
                           inputs_data,
                           sing_values = NULL,
                           input_params,
                           input_names,
                           data_server = NULL) {

  purrr::map(input_names, function(id){
    inputs_user[[id]] <- input[[id]]
    inputs_data[[id]]$input_type <- input_params$inputs[[id]]$input_type
    input_arg <- input_params$inputs[[id]]$input_params
    input_names_arg <- names(input_arg)

    input_arg <-  purrr::map(input_names_arg, function(arg) {
      conf_list <- input_arg[[arg]]
      conf_list <- internal_conditions(input = input,
                                       conf_list = conf_list,
                                       sing_values = sing_values,
                                       input_id = input_params$inputs[[id]]$id,
                                       input_params = input_params,
                                       data_server = data_server)
      conf_list
    })
    names(input_arg) <- input_names_arg
    inputs_data[[id]]$input_params <- c(list(inputId = id), input_arg)
  })
}
