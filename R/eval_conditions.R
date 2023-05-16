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


# equals: Checks if two values are equal.
# not_equals: Checks if two values are not equal.
# is_any_of: Checks if the first value is included in the second value, which is expected to be a set or collection.
# is_none_of: Checks if the first value is not included in the second value, which is expected to be a set or collection.
# has: Checks if the first value, expected to be a set or collection, includes the second value.
# contains: Checks if the first value, expected to be a string, contains the second value.
# any_contains: Checks if any element of the first value, expected to be a collection of strings, contains the second value.
# contained_in: Checks if the first value, expected to be a string, is contained in the second value.
# contained_in_all_of: Checks if the first value, expected to be a string, is contained in all elements of the second value, which is expected to be a collection of strings.
# contained_in_any_of: Checks if the first value, expected to be a string, is contained in any element of the second value, which is expected to be a collection of strings.
# contained_in_none_of: Checks if the first value, expected to be a string, is not contained in any element of the second value, which is expected to be a collection of strings.
# does_not_contain: Checks if the first value, expected to be a string, does not contain the second value.
# in: Checks if the first value is included in the second value, which is expected to be a set or collection.
# not_in: Checks if the first value is not included in the second value, which is expected to be a set or collection.
# in_range: Checks if the first value is within the range of the second value, which is expected to be a numeric range.
# is_between: Similar to in_range, but it excludes the endpoints of the range.
# less_than: Checks if the first value is less than the second value.
# is_within: Similar to in_range.
# is_before: Similar to less_than.
# is_after: Checks if the first value is greater than the second value.
# greater_than: Checks if the first value is greater than the second value.
# is_empty: Checks if the first value is empty or NULL.
# is_not_empty: Checks if the first value is not empty or NULL.


