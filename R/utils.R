
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
#' operations <- list(filter = list(col = "a", condition = "greater_than", arg = 2), setNames = list(col = "a", col_name = "b"))
#' perform_operations(df, operations) # returns a data frame with a > 2
#'

perform_operations <- function(df, operations) {

  if ("table_id" %in% names(operations)) {
    operations <- operations[-grep("table_id", names(operations))]
  }
  df <- purrr::reduce(names(operations), function(data, op_name) {
    op <- operations[[op_name]]
    if(op_name == "filter") {
      condition <- get_condition(op$condition)
      arg <- paste0("'", op$arg, "'")
      if (length(arg) > 1) {
        arg <-  paste0("c(",paste0(arg, collapse = ","), ")")
      }
      expr <- paste0(op$col, condition, arg)
      args <- list(rlang::parse_expr(expr))
    } else if (op_name %in% ls("package:base")) {
      if (op_name == "list") {
      args <- purrr::map(unique(data[[op$col_name]]), function(i) {
        data <- data |> dplyr::filter(!!dplyr::sym(op$col_name) %in% i)
        unique(data[[op$col]])
      })
      names(args) <- unique(data[[op$col_name]])
      } else {
      args <- list(x = data[[op$col]])
      }
    } else if (op_name == "setNames") {
      args <- list(object = data[[op$col]], nm = data[[op$col_name]])
    } else {
      args <- list(rlang::sym(op$col))
    }

    if ("extra" %in% names(op)) {
      args <- c(args, op$extra)
    }

    if(op_name %in% ls("package:dplyr")) {
      do.call(get(op_name, "package:dplyr"), c(list(data), args))
    } else if (op_name %in% ls("package:base")) {
      if (op_name == "list") {
        args
      } else {
      do.call(get(op_name, "package:base"), args)
      }
    } else if (op_name %in% ls("package:stats")) {
      do.call(get(op_name, "package:stats"), args)
    } else {
      stop(paste0("Operation ", op_name, " not found in dplyr or base packages."))
    }
  }, .init = df)
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
  if (is.null(range)) return()
  if (all("" %in% range)) return()

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


read_params <- function(path) {
  if (is.null(path)) stop("You must enter a path")
  ext <- substring(path, regexpr("\\.([[:alnum:]]+)$", path) + 1L)
  if (ext == "json")  file <- jsonlite::read_json(path)
  if (ext == "yaml") file <- yaml::read_yaml(path)
  file
}

