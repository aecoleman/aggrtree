#' Tree to Data.Frame
#'
#' Function to convert data.tree object back to a data.frame, with all attrs.
#' By using ToDataFrameTable, you would get only the leaf-level information.
#' ToDataFrameTree gives aggregate values calculated at the different levels of
#' the hierarchy as well, which is needed to compute values for metrics at each
#' of the levels.
#'
#' @param node a data.tree Node object
#' @param attrs character, optional. Attributes to be included in the output.
#' If not provided, will default to include all attributes present in the data.tree.
#' @param attributes_only logical, optional. If \code{TRUE}, the default, only
#' attribute columns and pathString are returned in the data.frame. If
#' \code{FALSE} columns added by the function \code{ToDataFrameTree} are also
#' returned. These include \code{name}, \code{levelName}, and \code{level},
#' which can be useful to have if you intend to rank nodes by level based on
#' some attribute (or function of attributes).
#'
#' @return data.frame
#' @export
#'
#' @importFrom data.tree ToDataFrameTree
#' @importFrom dplyr select
#'
tree_to_df <- function(node, ..., attrs = NULL, attributes_only = TRUE) {

  # Guard: Make sure that node is a data.tree
  stopifnot(is.Node(node))

  if (is.null(attrs)) {
    attrs <- node$fieldsAll
  }

  df_out <-
    do.call(
      data.tree::ToDataFrameTree,
      unlist(
        list(node,
             'name', 'level', 'pathString',
             as.list(attrs)), recursive = FALSE))

  if (attributes_only == TRUE) {
    df_out %>%
      dplyr::select(-name, -levelName, -level)
  } else {
    df_out
  }


}

#' Transpose Tree Data.Frame
#'
#' Transposes a data.frame such that the values in the column named 'pathString'
#' in the original data.frame become the new column names, and the column names
#' in the original data.frame become the new values in the column named
#' 'pathString'.
#'
#' Note that if the attribute columns are not all of the same type, they WILL be
#' coerced to the same type using the usual order of preference.
#' (logical to integer to numeric to character)
#'
#' @param df data.frame, should contain a column named 'pathString'
#'
#' @return data.frame
#' @export
#'
#' @importFrom tidyr gather spread
#'
transpose_tree_df <- function(df) {
  df %>%
    tidyr::gather(key = 'attr', value = 'value', -pathString) %>%
    tidyr::spread(key = pathString, value = 'value')
}


#' Is object a data.tree Node
#'
#' @param x object. If \code{x} is a list of length greater than 1, each element
#' of \code{x} will be checked individually. Otherwise, only \code{x} will be
#' checked.
#'
#' @return logical
#' @importFrom data.tree Node
#'
is.Node <- function(x) {

  # If x is a vector, apply this function to each element of that vector.
  # Otherwise, check whether x matches the type and class of some protypical

  if (is.list(x) && length(x) > 1L) {

    # Recursion happens here
    return(
      vapply(
        x,
        FUN = is.Node,
        FUN.VALUE = logical(1L)))

  } else {

    # Create a prototype to test input against.
    basic_node <- data.tree::Node$new('prototype')

    same_type <- identical(typeof(x), typeof(basic_node))

    same_class <- identical(class(x), class(basic_node))

    return(same_type & same_class)

  }

}