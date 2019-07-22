#' To Data.Frame All
#'
#' Function to convert data.tree object back to a data.frame, with all attrs.
#' By using ToDataFrameTable, you would get only the leaf-level information.
#' ToDataFrameTree gives aggregate values calculated at the different levels of
#' the hierarchy as well, which is needed to compute values for metrics at each
#' of the levels.
#'
#' @param tree data.tree
#' @param attrs character, optional
#'
#' @return data.frame
#' @export
#'
tree_to_df <- function(tree, attrs = NULL, minimal = TRUE) {

  if (is.null(attrs)) {
    attrs <- tree$fieldsAll
  }

  df_out <-
    do.call(
      data.tree::ToDataFrameTree,
      unlist(
        list(tree,
             'name', 'level', 'pathString',
             as.list(attrs)), recursive = FALSE))

  if (minimal == TRUE) {
    df_out %>%
      dplyr::select(-name, -levelName, -level)
  } else {
    df_out
  }


}

#' Pivot Tree Data.Frame
#'
#' @param df data.frame
#'
#' @return
#' @export
#'
pivot_tree_df <- function(df) {
  df %>%
    tidyr::gather(key = 'attr', value = 'value', -pathString) %>%
    tidyr::spread(key = pathString, value = 'value')
}
