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
