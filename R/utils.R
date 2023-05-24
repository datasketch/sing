
#' @export
available_inputs <- function() {
  update_info$input
}

#' @title Get condition operator
#'
#' @description This function takes a string specifying a condition and returns the corresponding operator.
#'
#' @param condition A character string specifying the condition to translate into an operator. This can be one of the following: "equals", "in", "distinct", "is_na", "is_empty", "is_not_empty", "less_than", "greater_than".
#'
#' @return This function returns a string representing the operator corresponding to the condition provided.
#'
#' @examples
#' get_condition("equals") # returns "=="
#'
get_condition <- function(condition) {
  switch(condition,
         "equals" = "==",
         "in" = "%in%",
         "distinct" = "!=",
         "is_na" = "is.na",
         "is_empty" = "is.null",
         "is_not_empty" = "!is.null",
         "less_than" = "<",
         "greater_than" = ">",
         stop(paste("Unknown condition:", condition))
  )
}


#' @title Perform operations on a data frame
#'
#' @description This function performs a series of operations on a data frame based on a list of operation names.
#'
#' @param df A data frame to perform the operations on.
#' @param operations A list of operations to perform. The operations should be named with the corresponding function names in dplyr package. For example, "filter", "select", "distinct", etc.
#'
#' @return This function returns a data frame that is the result of performing the operations on the original data frame.
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' operations <- list(filter = list(col = "a", condition = "greater_than", arg = 2))
#' perform_operations(df, operations) # returns a data frame with a > 2
#'

perform_operations <- function(df, operations) {
  if ("table_id" %in% names(operations)) {
    operations <- operations[-grep("table_id", names(operations))]
  }

  if ("unique" %in% names(operations)) {
    df <- unique(df[[operations$unique$col]])
  } else {
    df <- purrr::reduce(names(operations), function(data, op_name) {
      op <- operations[[op_name]]
      if(op_name == "filter") {
        condition <- get_condition(op$condition)
        expr <- paste0(op$col, condition, "'", op$arg, "'")
        args <- list(rlang::parse_expr(expr))
      } else {
        args <- list(rlang::sym(op$col))
      }
      do.call(get(op_name, "package:dplyr"), c(list(data), args))
    }, .init = df)
  }
  return(df)
}



# funcion para evaluar si en la configuracion de inputs se indica que se
# debe recorrer el archivo R6 con una info dada
hdbase_names <- function(what_tables, input_params, conf_list) {
  what_tables <- strsplit(what_tables, split = "\\.") |> unlist()
  conf_list <- input_params[[what_tables[1]]]
  conf_list <- purrr::map(names(conf_list), ~ {
    setNames(conf_list[[.x]]$slug, conf_list[[.x]]$name)
  }) |> unlist()
  conf_list
}


filter_ranges <- function(data, range, by) {
  if (is.null(data)) return()

  min_date <- min(data[[by]], na.rm = TRUE)
  max_date <- max(data[[by]], na.rm = TRUE)

  if (length(range) == 2) {
    if (min_date == range[1] & max_date == range[2]) {
      data_filter <- data
    } else {
      data_filter <- data |>
        dplyr::filter(!!dplyr::sym(by) >= range[1] &
                        !!dplyr::sym(by) <= range[2])
    }
  } else {
    data_filter <-  data |>
      dplyr::filter(!!dplyr::sym(by) == range)
  }
  data_filter

}
