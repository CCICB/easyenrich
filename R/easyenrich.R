#' Convert a Named List of Vectors into Contingency Tables
#'
#' This function takes a named list of character or factor vectors
#' and produces a list of contingency tables. Each table tests for
#' the presence or absence of a specific unique element across the groups.
#'
#' @param input_list A named list where each element is a character or factor vector.
#'
#' @return A named list of contingency tables (one per unique element).
#' Each contingency table shows counts of presence and absence of the element across groups.
#' Each table is a numeric matrix
#'
#' @examples
#' input <- list(
#'   colorectal = c("APC", "APC", "APC", "TP53", "APC"),
#'   melanoma = c("BRAF", "BRAF", "BRAF", "TP53"),
#'   breast = c("BRAF", "BRAF", "BRAF", "TP53")
#' )
#' list_to_contingency_tables(input)
#'
#' @export
list_to_contingency_tables <- function(input_list) {
  # Validate input
  if (is.null(names(input_list)) || any(nchar(names(input_list)) == 0)) {
    stop("`input_list` must be a named list with no unnamed elements")
  }

  # Find all unique elements across all groups
  unique_elements <- unique(unlist(input_list))

  # Fix for factors
  unique_elements <- if(is.factor(unique_elements)) levels(unique_elements) else unique_elements

  # Precompute simple counts for each group
  group_counts <- lapply(input_list, table)

  # Build a contingency table for each unique element
  contingency_list <- lapply(unique_elements, function(element) {

    # Count presence of `element` in each group
    element_present_counts <- vapply(group_counts, function(tbl) {
      if (element %in% names(tbl)) tbl[[element]] else 0
    }, FUN.VALUE = numeric(1))

    # Total items per group
    total_counts <- vapply(group_counts, sum, numeric(1))

    # Count absence of `element`
    element_absent_counts <- total_counts - element_present_counts

    # Assemble contingency matrix
    contingency_matrix <- rbind(
      present = element_present_counts,
      absent = element_absent_counts
    )
    # rownames(contingency_matrix) <- c(element, paste0("no ", element))

    contingency_matrix
  })

  names(contingency_list) <- unique_elements

  contingency_list
}

#' Compute Fisher Test on Contingency tables
#'
#' @param contingencies a named list of contingency tables
#' @param min_count only consider elements present at least this many times in one cohort.
#' @returns data.frame describing results of fisher test.
#' Includes columns 'present' (total number of elements observed in all cohorts and 'absent' total number of entries not matching that element)
#' @export
#'
#' @examples
#' # Step 1: Create a list of two or more vectors.
#' input <- list(
#'  colorectal = c("APC", "APC", "APC", "TP53", "APC"),
#'  melanoma = c("BRAF", "BRAF", "BRAF", "BRAF", "BRAF", "TP53", "APC")
#' )
#'
#' # Step 2: Create contingency tables
#' contingency_tables <- list_to_contingency_tables(input)
#'
#' # Step 3: Compute fisher p values & odds ratios
#' contingency_tables_to_fisher(contingency_tables)
#'
contingency_tables_to_fisher <- function(contingencies, min_count = 0){

  # Setup empty dataframe for early returns
  empty_df <- data.frame(
    element = character(0),
    p.value = numeric(0),
    odds_ratio = numeric(0),
    conf_level = numeric(0),
    conf.int.lower = numeric(0),
    conf.int.upper = numeric(0),
    null.value = numeric(0),
    alternative = character(0),
    fdr = numeric(0),
    present_colorectal = integer(0),
    absent_colorectal = integer(0),
    present_melanoma = integer(0),
    absent_melanoma = integer(0),
    total_present = integer(0),
    total_absent = integer(0),
    stringsAsFactors = FALSE
  )

  # Grab elements
  elements <- names(contingencies)

  # If contingencies list is empty return empty dataframe
  if(length(contingencies) ==0) return(empty_df)

  # Validate input
  if (is.null(elements) || any(nchar(elements) == 0)) {
    stop("`input_list` must be a named list with no unnamed elements")
  }

  for(contingency_table in contingencies) {
    if(!is.matrix(contingency_table)) stop("contingency tables must be a matrix")
    if(!is.numeric(contingency_table)) stop("contingency tables must be a numeric matrix")
    if(ncol(contingency_table) < 2) stop("contingency tables must have >=2 columns")
    if(nrow(contingency_table) < 2) stop("contingency tables must have >=2 rows")
    if(is.null(colnames(contingency_table))) stop("contingency tables must have column names")
    if(!all(nzchar(colnames(contingency_table)))) stop("contingency tables must have column names")
    if(is.null(rownames(contingency_table))) stop("contingency tables must have row names")
    if(!setequal(rownames(contingency_table), c("present", "absent"))) stop("contingency tables must have row names 'present' and 'absent'")
    if(!all(nzchar(rownames(contingency_table)))) stop("contingency tables must have row names")
  }


  # Filter for elements present >= min_count  times in one cohort
  if(min_count > 0){

    # Compute max count of each element in any cohort
    max_counts_in_any_cohort = vapply(contingencies, function(mx){ max(mx[rownames(mx)=="present",])}, FUN.VALUE = numeric(1))

    # Which elements are present enough times to compute fisher test
    elements_passing = names(max_counts_in_any_cohort)[max_counts_in_any_cohort >= min_count]

    # Filter contingencies for just those elements
    contingencies <- contingencies[names(contingencies) %in% elements_passing]

  }

    # If contingency table list is empty, early return empty dataframe
    if(length(contingencies) == 0){ return(empty_df) }


  # Compute total presence/absense counts per gene sums
  ls_sums_cols = lapply(contingencies, function(mx){ marginSums(mx, margin=1) })
  df_sums_cols = do.call("rbind", ls_sums_cols)
  # browser()
  colnames(df_sums_cols) <- paste0("total_", colnames(df_sums_cols))

  # Represent contingency table as vector so can annotate fisher df with it
  ls_contingency_vectors = lapply(contingencies, flatten_named_matrix)
  df_contingencies = do.call("rbind", ls_contingency_vectors)

  # Run Fisher Test
  ls_fisher = lapply(contingencies, function(mx) {
    fisher_to_df(stats::fisher.test(mx))
  })


  # Bind rows to create dataframe
  df_fisher <- do.call("rbind", ls_fisher)

  # Add fdr / other correction method
  df_fisher[["fdr"]] <- stats::p.adjust(df_fisher[["p.value"]], method = "fdr")

  # Add total present
  df_fisher <- cbind(df_fisher, df_contingencies, df_sums_cols)

  # Add Element
  df_fisher[["element"]] <- rownames(df_fisher)


  # Put element first
  df_fisher <- df_fisher[,c("element", setdiff(colnames(df_fisher), "element")), drop = FALSE]



  rownames(df_fisher) <- NULL

  return(df_fisher)
}

lpluck <- function(list, value){
  lapply(list, function(element){pluck(element, value)})
}

pluck <- function(x, value){
  if(value %in% c(colnames(x), names(x)))
    x[[value]]
  else
    NA
}

fisher_to_df <- function(fisher){
  if(!"htest" %in% class(fisher)) stop("expected the results of a fisher test, got a ", class(fisher))
  df <- data.frame(
    p.value = pluck(fisher, "p.value"),
    odds_ratio = pluck(fisher, "estimate"),
    conf_level = null_to_na(attr(pluck(fisher, "conf.int"), "conf.level")),
    conf.int.lower = pluck(fisher, "conf.int")[1],
    conf.int.upper = pluck(fisher, "conf.int")[2],
    null.value = pluck(fisher, "null.value"),
    alternative = pluck(fisher, "alternative")
  )

  rownames(df) <- NULL

  if(nrow(df) == 0) stop("fisher_to_df should never return an empty dataframe. Please create a new issue on github")
  return(df)
}


null_to_na <- function(x){
 if(is.null(x)) return(NA) else return(x)
}


