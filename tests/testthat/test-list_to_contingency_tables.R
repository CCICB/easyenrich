# test_that("Correct contingency tables are generated for typical input (character vectors)", {
#   # Define a sample input (from the provided documentation example)
#   input <- list(
#     colorectal = c("APC", "APC", "APC", "TP53", "APC"),
#     melanoma   = c("BRAF", "BRAF", "BRAF", "TP53"),
#     breast     = c("BRAF", "BRAF", "BRAF", "TP53")
#   )
#
#   # Expected unique elements: "APC", "TP53", "BRAF"
#   # For each unique element, we manually compute:
#   # For element "APC":
#   #   - colorectal: present 4, total 5 -> absent = 1
#   #   - melanoma: present 0, total 4 -> absent = 4
#   #   - breast: present 0, total 4 -> absent = 4
#   expected_APC <- rbind(present = c(colorectal = 4, melanoma = 0, breast = 0),
#                         absent  = c(colorectal = 1, melanoma = 4, breast = 4))
#
#   # For element "TP53":
#   #   - colorectal: present 1 (from one TP53), total 5 -> absent = 4
#   #   - melanoma: present 1, total 4 -> absent = 3
#   #   - breast: present 1, total 4 -> absent = 3
#   expected_TP53 <- rbind(present = c(colorectal = 1, melanoma = 1, breast = 1),
#                          absent  = c(colorectal = 4, melanoma = 3, breast = 3))
#
#   # For element "BRAF":
#   #   - colorectal: present 0, total 5 -> absent = 5
#   #   - melanoma: present 3, total 4 -> absent = 1
#   #   - breast: present 3, total 4 -> absent = 1
#   expected_BRAF <- rbind(present = c(colorectal = 0, melanoma = 3, breast = 3),
#                          absent  = c(colorectal = 5, melanoma = 1, breast = 1))
#
#   # Run the function
#   result <- list_to_contingency_tables(input)
#
#   # Check that the result is a named list with names equal to the unique elements (order may vary)
#   expect_true(is.list(result))
#   expect_setequal(names(result), c("APC", "TP53", "BRAF"))
#
#   # Due to potential ordering differences, we check each element's corresponding matrix by name
#   expect_equal(result[["APC"]], expected_APC)
#   expect_equal(result[["TP53"]], expected_TP53)
#   expect_equal(result[["BRAF"]], expected_BRAF)
# })

test_that("Works correctly when input vectors are factors", {
  input <- list(
    group1 = factor(c("x", "y", "x")),
    group2 = factor(c("y", "y", "z"))
  )

  # Unique elements: "x", "y", "z"
  # For "x":
  #   - group1: present = 2, total = 3, absent = 1.
  #   - group2: present = 0, total = 3, absent = 3.
  expected_x <- rbind(present = c(group1 = 2, group2 = 0),
                      absent  = c(group1 = 1, group2 = 3))

  # For "y":
  #   - group1: present = 1, total = 3, absent = 2.
  #   - group2: present = 2, total = 3, absent = 1.
  expected_y <- rbind(present = c(group1 = 1, group2 = 2),
                      absent  = c(group1 = 2, group2 = 1))

  # For "z":
  #   - group1: present = 0, total = 3, absent = 3.
  #   - group2: present = 1, total = 3, absent = 2.
  expected_z <- rbind(present = c(group1 = 0, group2 = 1),
                      absent  = c(group1 = 3, group2 = 2))

  result <- list_to_contingency_tables(input)

  expect_true(is.list(result))
  expect_setequal(names(result), c("x", "y", "z"))
  expect_equal(result[["x"]], expected_x)
  expect_equal(result[["y"]], expected_y)
  expect_equal(result[["z"]], expected_z)
})


test_that("Errors when input_list is not properly named", {

  # Test with completely unnamed list
  input1 <- list(c("A", "B"), c("B", "C"))
  expect_error(list_to_contingency_tables(input1),
               "`input_list` must be a named list with no unnamed elements")

  # Test with one missing name (empty string)
  input2 <- list(group1 = c("A", "B"), c("B", "C"))
  expect_error(list_to_contingency_tables(input2),
               "`input_list` must be a named list with no unnamed elements")
})

test_that("Handles groups with no occurrences of a specific element", {
  input <- list(
    group1 = c("alpha", "beta"),
    group2 = c("beta", "gamma"),
    group3 = c("gamma", "delta")
  )

  # For element "alpha": Only present in group1.
  # group1: present = 1, total = 2, absent = 1
  # group2: present = 0, total = 2, absent = 2
  # group3: present = 0, total = 2, absent = 2
  expected_alpha <- rbind(present = c(group1 = 1, group2 = 0, group3 = 0),
                          absent  = c(group1 = 1, group2 = 2, group3 = 2))

  result <- list_to_contingency_tables(input)
  expect_true("alpha" %in% names(result))
  expect_equal(result[["alpha"]], expected_alpha)
})


test_that("Output matrices have correct dimensions and row names", {
  input <- list(
    A = c("item1", "item2", "item1"),
    B = c("item2", "item3")
  )

  result <- list_to_contingency_tables(input)

  # Each output matrix should be 2 x N where N is the number of groups
  for (tbl in result) {
    expect_equal(dim(tbl), c(2, length(input)))
  }

  # Check row names (although commented-out code in function suggests using "present" and "absent")
  # Here we check the row names are "present" and "absent"
  for (tbl in result) {
    expect_equal(rownames(tbl), c("present", "absent"))
  }
})
