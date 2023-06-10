# List of Condition Functions
#
# This list contains functions for various conditions that can be evaluated
# between two values.
#' @usage conditions
#" @format A named list of 23 functions.
conditions <- list(
  equals = function(x, y) x == y,
  not_equals = function(x, y) x != y,
  is_any_of = function(x, y) x %in% y,
  is_none_of = function(x, y) !x %in% y,
  has = function(x, y) !is.null(x) && y %in% x,
  contains = function(x, y) grepl(y, x),
  any_contains = function(x, y) any(grepl(y, x)),
  contained_in = function(x, y) grepl(x, y),
  contained_in_all_of = function(x, y) all(grepl(x, y)),
  contained_in_any_of = function(x, y) any(grepl(x, y)),
  contained_in_none_of = function(x, y) !all(grepl(x, y)),
  does_not_contain = function(x, y) !grepl(y, x),
  `in` = function(x, y) x %in% y,
  not_in = function(x, y) !x %in% y,
  in_range = function(x, y) x <= max(y) && x >= min(y),
  is_between = function(x, y) x < max(y) && x > min(y),
  less_than = function(x, y) x < y,
  is_within = function(x, y) x <= max(y) && x >= min(y),
  is_before = function(x, y) x < y,
  is_after = function(x, y) x > y,
  greater_than = function(x, y) x > y,
  is_empty = function(x, y) is.null(x) || length(x) == 0,
  is_not_empty = function(x, y) !is.null(x) && length(x) > 0
)


external_condition <- list(
  any_of = function(logical_values) any(logical_values),
  equals = function(logical_values) all(logical_values),
  all_false = function(logical_values) !all(logical_values)
)




#' Evaluate a Condition
#'
#' This function takes two values and a condition, and evaluates the condition on the values.
#'
#' @param element The first input value. The subject of the condition.
#' @param condition The condition to evaluate. Should be a string matching one of the available conditions.
#' @param comparison_value The second input value. The value against which the condition is evaluated.
#'
#' @return The result of the evaluation. The return type depends on the condition.
#' @examples
#' eval_conditions(5, "equals", 5)  # Returns TRUE
#' eval_conditions(5, "greater_than", 3)  # Returns TRUE
#' eval_conditions(5, "less_than", 3)  # Returns FALSE
#'
#' @export
eval_conditions <- function(element, condition, comparison_value) {
  if (!condition %in% names(conditions)) {
    stop(paste0("La condición '", condition, "' no es reconocida"))
  }
  conditions[[condition]](element, comparison_value)
}

#' Primary Conditions Evaluation
#'
#' This function evaluates a set of conditions for a specific inputs. The purpose of this function is to determine
#' whether a specific condition is met for the input. The conditions are evaluated based on the 'condition' argument
#' and, if necessary, a 'comparison_value'.
#'
#' @param input A list of all the inputs in a Shiny application, usually provided within a reactive context.
#' @param input_id Character. The ID of the input to evaluate.
#' @param condition Character. The condition to evaluate. This can be any condition that can be evaluated by the
#'   'eval_conditions' function, including 'is_null' which checks if the input value is NULL.
#' @param comparison_value The value to compare the input value to, if necessary. This is used by the 'eval_conditions'
#'   function to evaluate conditions that require a comparison.
#'
#' @return This function returns TRUE if the input value meets the specified condition.
#' @keywords internal
primary_conditions <- function(input, input_id, condition, comparison_value) {
  value_input <- input[[input_id]]
  result_condition <- TRUE
  if (condition == "is_null") {
    result_condition <- is.null(value_input)
  } else {
    if (!is.null(value_input)) {
      result_condition <- eval_conditions(value_input, condition, comparison_value)
    }
  }
  result_condition
}

#' @keywords internal
secondary_conditions <- function(logical_values, condition) {
  if (is.null(logical_values)) return()
  external_condition[[condition]](logical_values)
}



#' @title Evaluate filter conditions in an input
#'
#' @description This function evaluates whether an input should be filtered based on its filter conditions.
#' If a filter condition is present and its argument is non-null and non-empty, the input will be filtered accordingly.
#' If the filter argument is null or empty, the function sets a default condition to consider only unique values of the input.
#' If a filter condition is not present, the function does nothing.
#'
#' @param input A list of inputs.
#' @param conf_list A configuration list with filter conditions.
#' @param conditions_into_input A list of conditions to evaluate for the input.
#' @param input_id A character string specifying the ID of the input to evaluate. Defaults to NULL.
#'
#' @return This function returns the list of conditions for the input after evaluating the presence and value of its filter condition.
#' If a filter condition is present and its argument is non-null and non-empty, the corresponding filter condition is returned.
#' If the filter argument is null or empty, a default condition considering only unique values of the input is returned.
#' If a filter condition is not present, the function returns NULL.
#'

evaluate_filter_into_input <- function(input,
                                       conf_list,
                                       conditions_into_input,
                                       input_id = NULL) {
  if ("filter" %in% names(conditions_into_input)) {
    if (grepl("_input",conditions_into_input$filter$arg)) {
      arg <- input[[conf_list$what$filter$arg]]
      id_alt <- input_id
      if (is.null(id_alt)) return()
      if (all(is.null(arg)) || all(arg == "")) {
        if (length(id_alt) == 1) {
          conditions_into_input <- list("unique" = list("col" = id_alt))
        } else {
          conditions_into_input <- list("arrange" = list("col" = id_alt[length(id_alt)]),
                                        "distinct" = list("col" = id_alt[1], "extra" = list(".keep_all" = TRUE)),
                                        "setNames" = list("col" = id_alt[1], "col_name" = id_alt[2]))
        }
      } else {
        conditions_into_input$filter$arg <- arg
      }
    }
  }
  conditions_into_input
}




#' @title Evaluate data-dependent conditions
#'
#' @description This function evaluates the conditions that depend on the data stored in the database for a given input.
#'
#' @param input A list of inputs.
#' @param conf_list A list of configuration for each input.
#' @param conditions_into_input A list of conditions to evaluate for the input.
#' @param input_id A character string specifying the ID of the input to evaluate. Defaults to NULL.
#' @param data_server A list of data frames, each named with a table_id.
#'
#' @return This function returns the list of configuration for the input after evaluating the data
data_dependent_conditions <- function(input,
                                      conf_list,
                                      conditions_into_input,
                                      input_id = NULL,
                                      data_server = NULL) {
  if ("table_id" %in% names(conditions_into_input)) { #si tiene table id es porq algun argumento requiere de la base de datps
    if (is.null(input[["what_table_input"]])) return()
    if (input[["what_table_input"]] != conditions_into_input$table_id) return()
    data_select <- data_server[[conditions_into_input$table_id]]
    conditions_into_input <- evaluate_filter_into_input(input,
                                                        conf_list,
                                                        conditions_into_input,
                                                        input_id = input_id)
    conf_list <- perform_operations(data_select, conditions_into_input)
  }
  conf_list
}

#' Internal Condition Evaluation
#'
#' This function evaluates whether the condition is a reactive value, a database access or
#' a list of conditions. The 'sing_values' parameter indicates the name of the reactive value
#' that contains the specification of the desired value.
#'
#' @param input A list of inputs.
#' @param conf_list A list of configurations.
#' @param sing_values The reactive values.
#' @param input_id The id of the input.
#' @param input_params Parameters of the input.
#' @param data_server The data server.
#' @return The configuration list.
#' @export
internal_conditions <- function(input,
                                conf_list,
                                sing_values = NULL,
                                input_id = NULL,
                                input_params,
                                data_server = NULL) {

  if (!is.null(names(conf_list))) {
    if ("what" %in% names(conf_list)) {
      if (length(conf_list$what) == 1) {
        if (grepl("\\b\\.\\b", conf_list$what)) {
          conf_list <- hdbase_names(what_tables = conf_list$what,
                                    input_params = input_params, conf_list = conf_list)
        } else {
          conf_list <- sing_values[[conf_list$what]]
        }
      } else {
        conditions_into_input <- conf_list$what
        conf_list <- data_dependent_conditions(input = input,
                                               conf_list = conf_list,
                                               conditions_into_input = conditions_into_input,
                                               input_id = input_id,
                                               data_server = data_server)

      }
    }
  }

  conf_list
}

