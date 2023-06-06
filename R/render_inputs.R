#' @export
render_sing <- function(session,
                        input,
                        bd,
                        input_params,
                        sing_values) {

  data_server <- reactiveValues()
  inputs_user <- reactiveValues()
  inputs_data <- reactiveValues()

  if (class(input_params) == "character") {
    input_params <- read_params(input_params)
  }

  id_inputs <- setdiff(names(input_params$inputs), c("exclude", "include"))


  data_filter <- reactive({
    if (is.null(input[["what_table_input"]]) ||
        is.na(input[["what_table_input"]]) || input[["what_table_input"]] == "") return()

    df <- data_server[[input[["what_table_input"]]]]
    dic <- bd$hdtables[[input[["what_table_input"]]]]$dic

    df <- purrr::reduce(id_inputs, function(df, id) {
      if (id != "what_table_input" && !is.null(inputs_user[[id]])) {
        name_var <- input_params$inputs[[id]]$id
        info_var <- dic |> dplyr::filter(id %in% name_var)
        if (nrow(info_var) == 0) return(df)
        list_filters <- inputs_user[[id]]
        names(list_filters) <- name_var

        if (info_var$hdtype %in% c("Cat", "Yea", "Num")) {
          df <- df |> dplyr::filter(!!dplyr::sym(name_var) %in% inputs_user[[id]])
        }

        if (info_var$hdtype == "Dat") {
          df <- df |> filter_ranges(range = inputs_user[[id]], by = info_var$id)
        }
      }
      df
    }, .init = df)

    df_select <- NULL
    if (!is.null(df)) {
      if (nrow(df) > 0) {
        if ("data_filter" %in% names(input_params)) {
          data_filter_cond <- input_params$data_filter
          df_select <- lapply(names(data_filter_cond), function(d) {
            if (input[["what_table_input"]] != d) return(df)
            conf_list <- data_filter_cond[[d]]
            if ("arg" %in% names(conf_list[[1]])) {
              conf_list[[1]]$arg <- input[[conf_list[[1]]$arg]]
            }
            perform_operations(df, operations = conf_list)
          })[[1]]
        }
        df_select <- theme_viz_func(df = df_select,
                             input = input,
                             input_params = input_params,
                             sing_values=sing_values)
      }
    }


    list(
    data_filter = df,
    opts_viz = df_select
    )

  })





  observe({

   data_server$data_filter <- data_filter()$data_filter
    data_server$opts_viz <- data_filter()$opts_viz

    if (!is.null(input[["what_table_input"]])) {
      if (input[["what_table_input"]] != "") {
        data_server[[input[["what_table_input"]]]] <- bd$hdtables[[input[["what_table_input"]]]]$data
      }
    }

    pre_inp <- prepare_inputs(input = input,
                              inputs_data = inputs_data,
                              input_params = input_params,
                              input_names = id_inputs,
                              sing_values = sing_values,
                              data_server = data_server)


    purrr::map(id_inputs, function(id) {
      inputs_user[[id]] <- input[[id]]
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
        pass_condition <- purrr::map(input_depend, function(input_depend) {
          if (is.null(input[[input_depend]])) pass_condition <- TRUE
          sing:::primary_conditions(input, input_id = input_depend,
                                    condition = names(input_conditions[[input_depend]]),
                                    comparison_value = input_conditions[[input_depend]][[names(input_conditions[[input_depend]])]])

        }) |> unlist()
        pass_condition <- all(pass_condition)
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
