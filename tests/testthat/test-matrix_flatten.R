test_that("Correctly flattens a standard matrix", {
  mx <- matrix(
    c(0, 5, 3, 1),
    ncol = 2L,
    dimnames = list(c("present", "absent"), c("colorectal", "melanoma"))
  )

  result <- flatten_named_matrix(mx)

  # Expected result is produced in column-major order:
  # first column: "present_colorectal", "absent_colorectal"
  # second column: "present_melanoma", "absent_melanoma"
  expected <- c(0, 5, 3, 1)
  names(expected) <- c("present_colorectal", "absent_colorectal", "present_melanoma", "absent_melanoma")

  expect_equal(result, expected)
})

test_that("Handles custom separator correctly", {
  mx <- matrix(
    c(1, 2, 3, 4),
    ncol = 2L,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )

  result <- flatten_named_matrix(mx, sep = "-")

  expected <- c(1, 2, 3, 4)
  names(expected) <- c("r1-c1", "r2-c1", "r1-c2", "r2-c2")

  expect_equal(result, expected)
})

test_that("Handles a matrix with one row", {
  mx <- matrix(
    c(10, 20, 30),
    ncol = 3L,
    dimnames = list("onlyRow", c("c1", "c2", "c3"))
  )

  result <- flatten_named_matrix(mx)

  # For one row, names are: "onlyRow_c1", "onlyRow_c2", "onlyRow_c3"
  expected <- c(10, 20, 30)
  names(expected) <- c("onlyRow_c1", "onlyRow_c2", "onlyRow_c3")

  expect_equal(result, expected)
})

test_that("Handles a matrix with one column", {
  mx <- matrix(
    c(7, 8, 9),
    ncol = 1L,
    dimnames = list(c("r1", "r2", "r3"), "onlyCol")
  )

  result <- flatten_named_matrix(mx)

  # For one column, names are: "r1_onlyCol", "r2_onlyCol", "r3_onlyCol"
  expected <- c(7, 8, 9)
  names(expected) <- c("r1_onlyCol", "r2_onlyCol", "r3_onlyCol")

  expect_equal(result, expected)
})

test_that("Throws an error when x is not a matrix", {
  not_a_matrix <- 1:5
  expect_error(flatten_named_matrix(not_a_matrix), "Input 'x' must be a matrix")
})

test_that("Throws an error when matrix has no column names", {
  mx <- matrix(
    c(1, 2, 3, 4),
    ncol = 2L
  )
  rownames(mx) <- c("r1", "r2")

  expect_error(flatten_named_matrix(mx), "Matrix must have non-empty column names")
})

test_that("Throws an error when matrix has no row names", {
  mx <- matrix(
    c(1, 2, 3, 4),
    ncol = 2L
  )
  colnames(mx) <- c("c1", "c2")

  expect_error(flatten_named_matrix(mx), "Matrix must have non-empty row names")
})

test_that("Throws an error when matrix has empty column names", {
  mx <- matrix(
    c(1, 2, 3, 4),
    ncol = 2L,
    dimnames = list(c("r1", "r2"), c("", "c2"))
  )
  expect_error(flatten_named_matrix(mx), "Matrix must have non-empty column names")
})

test_that("Throws an error when matrix has empty row names", {
  mx <- matrix(
    c(1, 2, 3, 4),
    ncol = 2L,
    dimnames = list(c("r1", ""), c("c1", "c2"))
  )
  expect_error(flatten_named_matrix(mx), "Matrix must have non-empty row names")
})
