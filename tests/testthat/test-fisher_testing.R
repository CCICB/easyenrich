# Define a basic valid input
example_input <- list(
  APC = structure(c(4, 1, 0, 4), dim = c(2L, 2L), dimnames = list(
    c("present", "absent"), c("colorectal", "melanoma")
  )),
  TP53 = structure(c(1, 4, 1, 3), dim = c(2L, 2L), dimnames = list(
    c("present", "absent"), c("colorectal", "melanoma")
  )),
  BRAF = structure(c(0, 5, 3, 1), dim = c(2L, 2L), dimnames = list(
    c("present", "absent"), c("colorectal", "melanoma")
  ))
)

expected_names <- c(
  "element", "p.value", "odds_ratio", "conf_level", "conf.int.lower",
  "conf.int.upper", "null.value", "alternative", "fdr",
  "present_colorectal", "absent_colorectal",
  "present_melanoma", "absent_melanoma",
  "total_present", "total_absent"
)

test_that("contingency_tables_to_fisher returns correct structure for normal input", {
  result <- contingency_tables_to_fisher(example_input)

  expect_s3_class(result, "data.frame")
  expect_named(result, expected_names)
  expect_equal(nrow(result), 3)
  expect_equal(result$element, c("APC", "TP53", "BRAF"))
})

test_that("contingency_tables_to_fisher returns empty data.frame with correct columns for empty input", {
  result <- contingency_tables_to_fisher(list())

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, expected_names)
})

test_that("contingency_tables_to_fisher returns empty data.frame when min_count filters out all", {
  result <- contingency_tables_to_fisher(example_input, min_count = 999)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, expected_names)
})

test_that("contingency_tables_to_fisher throws error on invalid input types", {
  bad_input <- list(
    bad = matrix(1:4, 2, 2)
  )
  # No dimnames
  expect_error(contingency_tables_to_fisher(bad_input), "contingency tables must have column names")

  bad_input2 <- list(
    bad = structure(1:4, dim = c(2, 2), dimnames = list(
      c("present", "absent"), c("", "melanoma")
    ))
  )
  expect_error(contingency_tables_to_fisher(bad_input2), "contingency tables must have column names")

  bad_input3 <- list(
    bad = structure(1:4, dim = c(2, 2), dimnames = list(
      c("present", "something_else"), c("colorectal", "melanoma")
    ))
  )
  expect_error(contingency_tables_to_fisher(bad_input3), "contingency tables must have row names 'present' and 'absent'")
})

test_that("contingency_tables_to_fisher throws error if input list is not properly named", {
  unnamed_input <- list(
    structure(c(4, 1, 0, 4), dim = c(2L, 2L), dimnames = list(
      c("present", "absent"), c("colorectal", "melanoma")
    ))
  )
  expect_error(contingency_tables_to_fisher(unnamed_input), "`input_list` must be a named list")
})
