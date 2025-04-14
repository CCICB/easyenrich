#' Flatten a Named Matrix to a Named Vector
#'
#' Converts a matrix with row and column names into a named vector. The names in the resulting vector are
#' constructed by concatenating the corresponding row and column names using a specified separator.
#'
#' @param x A matrix with non-empty row and column names. The input should be a valid matrix.
#' @param sep A character string used as the delimiter between row and column names in the constructed names.
#'   Defaults to `"_"`
#'
#' @return A vector representing the matrix elements with names formatted as "<row><sep><column>".
#'
#' @details The conversion preserves the column-major order inherent in R matrices. For example, if the
#'   matrix is constructed with `byrow = FALSE`, the resulting vector will list all elements of the first
#'   column (with names based on the corresponding rows and that column name), followed by the second column, etc.
#'
#' @export
#'
#' @examples
#' mx <- matrix(
#'   c(0, 5, 3, 1), ncol = 2L,
#'   dimnames = list(c("present", "absent"), c("colorectal", "melanoma"))
#' )
#' flatten_named_matrix(mx)
flatten_named_matrix <- function(x, sep = "_") {

  # Validate that the input is a matrix
  if (!is.matrix(x)) {
    stop("Input 'x' must be a matrix.")
  }

  # Validate that column names exist and are non-empty
  if (is.null(colnames(x)) || !all(nzchar(colnames(x)))) {
    stop("Matrix must have non-empty column names.")
  }

  # Validate that row names exist and are non-empty
  if (is.null(rownames(x)) || !all(nzchar(rownames(x)))) {
    stop("Matrix must have non-empty row names.")
  }

  # Retrieve row and column names, and dimensions
  rows <- rownames(x)
  cols <- colnames(x)
  nrows <- nrow(x)
  ncols <- ncol(x)

  # Flatten the matrix into a vector (column-major order)
  xvec <- as.vector(x)

  # Generate names for each element: replicate row names for each column and
  # repeat column names for each row to match the column-major flattening order
  names(xvec) <- paste(rep(rows, times = ncols), rep(cols, each = nrows), sep = sep)

  return(xvec)
}
