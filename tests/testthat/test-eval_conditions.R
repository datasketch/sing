test_that("Eval conditions", {

  # equals: Checks if two values are equal.
  value_result <- eval_conditions(3, "equals", 5)
  expect_false(value_result)

  # not_equals: Checks if two values are not equal.

  value_result <- eval_conditions(c("a", "b"), "not_equals", c("z", "b"))
  expect_type(value_result, "logical")
  value_result <- eval_conditions(5, "not_equals", 8)
  expect_true(value_result)


  # is_any_of: Checks if the first value is included in the second value,
  # which is expected to be a set or collection.
  value_result <- eval_conditions(c("a", "b"), "is_any_of", c("a", "b"))
  expect_type(value_result, "logical")
  value_result <- eval_conditions("a", "is_any_of", c("a", "b"))
  expect_true(value_result)

  # is_none_of: Checks if the first value is not included in the second value,
  # which is expected to be a set or collection.
  value_result <- eval_conditions(c("a", "b"), "is_none_of", c("a", "b"))
  expect_type(value_result, "logical")
  value_result <- eval_conditions(c("z", "z"), "is_none_of", c("a", "b"))
  expect_type(value_result, "logical")

  # has: Checks if the first value, expected to be a set or collection,
  # includes the second value.
  value_result <- eval_conditions("a", "has", "b")
  expect_false(value_result)
  value_result <- eval_conditions("b", "has", "b")
  expect_true(value_result)


  # contains: Checks if the first value, expected to be a string, contains the second value.
  value_result <- eval_conditions("a", "has", "b")
  expect_false(value_result)

  # any_contains: Checks if any element of the first value, expected to be a collection of strings, contains the second value.
  value_result <- eval_conditions("a", "has", "b")
  expect_false(value_result)


  # contained_in: Checks if the first value, expected to be a string,
  # is contained in the second value.
  value_result <- eval_conditions("a", "contained_in", c("b", "a"))
  expect_type(value_result, "logical")
  value_result <- eval_conditions("z", "contained_in", c("b", "a"))
  expect_type(value_result, "logical")

  # contained_in_all_of: Checks if the first value, expected to be a string, is contained in all elements of the second value, which is expected to be a collection of strings.
  value_result <- eval_conditions("a", "contained_in_all_of", c("b", "a"))
  expect_false(value_result)
  value_result <- eval_conditions("a", "contained_in_all_of", c("b", "z"))
  expect_false(value_result)

  # contained_in_any_of: Checks if the first value, expected to be a string, is contained in any element of the second value, which is expected to be a collection of strings.
  value_result <- eval_conditions("a", "contained_in_any_of", c("a", "b", "c"))
  expect_true(value_result)
  value_result <- eval_conditions("b", "contained_in_any_of", c("a", "z", "c", "b"))
  expect_true(value_result)

  # contained_in_none_of: Checks if the first value, expected to be a string, is not contained in any
  # element of the second value, which is expected to be a collection of strings.
  value_result <- eval_conditions("a", "contained_in_none_of", c("b", "z", "a"))
  expect_true(value_result)

  # does_not_contain: Checks if the first value, expected to be a string, does
  # not contain the second value.
  value_result <- eval_conditions("a", "does_not_contain", "b")
  expect_true(value_result)
  value_result <- eval_conditions(c("4", "d"), "does_not_contain", c("b"))
  expect_type(value_result, "logical")

  # in: Checks if the first value is included in the second value,
  # which is expected to be a set or collection.
  value_result <- eval_conditions(c("a", "b"), "in", "b")
  expect_type(value_result, "logical")
  value_result <- eval_conditions(c("a", "b"), "in", c("a", "b"))
  expect_type(value_result, "logical")
  value_result <- eval_conditions("a", "in", c("a", "b"))

  # not_in: Checks if the first value is not included in the second value, which
  # is expected to be a set or collection.
  value_result <- eval_conditions("a", "not_in", "b")
  expect_true(value_result)
  value_result <- eval_conditions("a", "not_in", c("b", "a"))
  expect_false(value_result)


  # in_range: Checks if the first value is within the range of the second value,
  # which is expected to be a numeric range.
  value_result <- eval_conditions(5, "in_range", 4:9)
  expect_true(value_result)
  value_result <- eval_conditions(15, "in_range", 4:9)
  expect_false(value_result)

  # is_between: Similar to in_range, but it excludes the endpoints of the range.
  value_result <- eval_conditions(10, "is_between", 7:15)
  expect_true(value_result)
  expect_error(eval_conditions(10:15, "is_between", 7:15))



  # less_than: Checks if the first value is less than the second value.
  value_result <- eval_conditions(5, "less_than", 10)
  expect_true(value_result)
  value_result <- eval_conditions(15, "less_than", 10)
  expect_false(value_result)

  # is_within: Similar to in_range.
  value_result <- eval_conditions(6, "is_within", 5)
  expect_false(value_result)
  value_result <- eval_conditions(6, "is_within", 1:7)
  expect_true(value_result)

  # is_before: Similar to less_than.
  value_result <- eval_conditions(7, "is_before", 3)
  expect_false(value_result)
  value_result <- eval_conditions(7, "is_before", 13)
  expect_true(value_result)



  # is_after: Checks if the first value is greater than the second value.
  value_result <- eval_conditions(10, "is_after", 100)
  expect_false(value_result)
  value_result <- eval_conditions(100, "is_after", 10)
  expect_true(value_result)


  # greater_than: Checks if the first value is greater than the second value.
  value_result <- eval_conditions(300, "greater_than", 3)
  expect_true(value_result)

  # is_empty: Checks if the first value is empty or NULL.
  value_result <- eval_conditions(NULL, "is_empty")
  expect_true(value_result)

  # is_not_empty: Checks if the first value is not empty or NULL.
  value_result <- eval_conditions("a", "is_not_empty")
  expect_true(value_result)
})
