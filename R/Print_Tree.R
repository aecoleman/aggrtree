#' Print Tree
#'
#' @param tree data.tree
#' @param attrs character, vector of node attributes whose values are to be
#' printed
#'
#' @return data.tree (invisibly)
#' @export
#'
#' @import data.tree
#'
Print_Tree <- function(tree, attrs = NULL) {

  if (is.null(attrs)) {
    attrs <- tree$fieldsAll
  }

  args <- unlist(
    list(tree, as.list(attrs)),
    recursive = FALSE)

  do.call(print, args)

  # Return tree invisibly, so this can be used in a pipe if desired
  return(invisible(tree))

}

