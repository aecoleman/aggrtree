#' Make Test Data.Frame
#'
#' Internal function that constructs a test data.frame for testing purposes
#'
#' @return data.frame
#'
.Make_Test_DF <- function() {

  isolate_vars <- .Make_Arbitrary_Hierarchy(max_depth = 3L, child_opts = 0L:3L)
  over_vars <- .Make_Arbitrary_Hierarchy(max_depth = 2L, child_opts = 0L:3L)

  df <- expand.grid(
    isolate_var = isolate_vars,
    over_var    = over_vars,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)

  df$measure_1 <- rbinom(n = nrow(df), size = 10L, prob = runif(n = 1))
  df$measure_2 <- runif(n = nrow(df)) * 10
  df$measure_3 <- runif(n = nrow(df)) - 0.5
  df$measure_D <- sample(letters, size = nrow(df), replace = TRUE)

  df

}

#' Make Arbitrary Hierarchy
#'
#'  Function to create an arbitrary hierarchy, primarily for the purposes of
#'  creating data for tests.
#'
#' @param max_depth integer, the depth at which to stop. Note that the root is
#' @param depth integer, optional, the current depth. This is primarily intended
#' to be used internally, as this function uses recursion to generate the nodes.
#' @param num_children_range integer vector, the min and max of this vector is
#' used to determine the possible numbers of children that each parent can have.
#' Integers within this range will be sampled in order to generate the children.
#' @param child_opts integer vector of any length, optional. If supplied,
#' this supercedes \code{num_children_range}, and is used to generate children.
#' @param parent_paths character, optional, the paths of the parents. Primarily
#' used internally.
#' @param name_mode character, can be any of 'ALTERNATE', 'alternate', 'LETTERS', 'letters', or 'numerals'. Both varieties of Alternate will result in names that alternate between using letters and using numerals, where ALTERNATE uses capitals and alternate uses lower case.
#' @param force_full_tree logical, should the tree be forced to be full (all leaf nodes at the maximum depth)?
#'
#' @return character, pathStrings for use in a data.tree structure
#' @export
#'
#' @examples
#'
#' # Make a full binary tree
#' .Make_Arbitrary_Hierarchy(depth = 3L, child_opts = 2L, parent_paths = '0', name_mode = 'ALTERNATE', force_full_tree = TRUE)
#'
.Make_Arbitrary_Hierarchy <- function(max_depth = 1L, depth = NULL, num_children_range = c(min = 1L, max = 5L), child_opts = NULL, parent_paths = NULL, name_mode = c('ALTERNATE', 'alternate', 'LETTERS', 'letters', 'numerals'), force_full_tree = TRUE) {

  name_mode <- name_mode[1]

  if (is.null(depth)) {
    depth <- 1L
  }

  # If depth is 1, make the root name. For LETTERS the default root is 'O', and
  # for letters it is 'o'. In all other cases, the default root is '0'.
  if (depth == 1L && is.null(parent_paths)) {
    parent_paths <-
      switch(name_mode,
             LETTERS = 'O',
             letters = 'o',
             '0')
  } else if (depth == 1L && length(parent_paths) > 1L) {
    parent_paths <- parent_paths[1]
  }

  # Check the depth of each parent. If a parent isn't at depth - 1L, then that
  # parent's lineage must have ended, and we should avoid giving it children
  # during this and future iterations.
  parent_depth <- stringr::str_count(parent_paths, '/')

  stopifnot(is.logical(force_full_tree))

  # If both child_opts and num_children_range are NULL, throw an error. Otherwise, check if child_opts is null. If it is, then num_children_range must have been supplied, and we will use it to generate child_opts.
  if (is.null(child_opts) && (missing(num_children_range) || is.null(num_children_range))) {
    stop('Either child_opts or num_children_range must be supplied!')
  } else if (is.null(child_opts)) {
    # Determine, the possible options for number of children
    child_opts <-
      as.integer(
        seq(from = max(force_full_tree, min(num_children_range)),
            to = max(num_children_range), by = 1L))
  }

  if (length(child_opts) > 1L) {
    # Determine the number of children that each parent will have
    num_children <-
        sample(
          child_opts,
          size = length(parent_paths), replace = TRUE)
  } else {
    num_children <- rep(child_opts, length(parent_paths))
  }

  # If we haven't reached max depth yet, but all parents are to have no
  # children, select one parent whose lineage is not ended to have one child.
  if (depth < max_depth && all(num_children) == 0L) {

    surviving_lineages <- which(parent_depth == depth - 1L)

    if (length(surviving_lineages) == 1L) {
      num_children[surviving_lineages] <- 1L
    } else {
      num_children[sample(surviving_lineages)] <- 1L
    }

  }

  pathString <-
    purrr::map2(
      parent_paths,
      num_children,
      .Make_Node_Paths,
      name_mode) %>%
    unlist()

  if (depth < max_depth) {
    # Recurse until max depth is reached.
    # We only supply child_opts, because we will have already found it.
    pathString <-
      .Make_Arbitrary_Hierarchy(
        parent_paths    = pathString,
        max_depth       = max_depth,
        depth           = depth + 1L,
        child_opts      = child_opts,
        force_full_tree = force_full_tree,
        name_mode       = name_mode)

  }

  pathString

}


.Make_Node_Paths <- function(parent_path, num_children, name_mode = c('ALTERNATE', 'alternate', 'LETTERS', 'letters', 'numerals')) {

  # If the parent is to have no children, return the parent_path
  if (num_children == 0L) {
    return(parent_path)
  }

  parent_name <- stringr::str_extract(parent_path, '[^/]+$')

  # First, if the name_mode is ALTERNATE or alternate, determine what mode
  # should be used at this depth.
  name_mode <-
    switch(name_mode,
           ALTERNATE = {ifelse(stringr::str_detect(parent_name, '[0-9]$'), 'LETTERS', 'numerals')},
           alternate = {ifelse(stringr::str_detect(parent_name, '[0-9]$'), 'letters', 'numerals')},
           name_mode)

  if (num_children > 25L && name_mode %in% c('LETTERS', 'letters')) {
    err_msg <-
      sprintf(
        'Cannot support more than 25 children for a single parent using name_mode %s. Consider reducing the number of children per parent or using name_mode \'numerals\'.', name_mode )

    stop(err_msg)
  }

  # Index of the letter 'O', which will be omitted from the possible results
  idx_o <- 15L

  # Generate the names of the children, which will be appended to the parent_name
  child_names <-
    switch(name_mode,
           LETTERS = {LETTERS[-idx_o][seq_len(num_children)]},
           letters = {letters[-idx_o][seq_len(num_children)]},
           numerals = {
             sprintf(
               # If we have greater than 9 children, pad with zeros so that all
               # child_names for this parent are of the same length.
               paste0('%0', floor(log10(num_children + 1L)) + 1L, 'd'),
               seq_len(num_children))})

  # Construct the pathStrings to be returned
  pathStrings <-
    paste0(parent_path, '/', parent_name, child_names)

  return(pathStrings)

}