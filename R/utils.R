
#' @export
available_inputs <- function() {
  update_info$input
}


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


# funcion que ejecuta cada posible operacion
# que se indica en la entrada de la configuracion de inputs

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



