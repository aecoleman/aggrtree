#' Aggregate Attributes
#'
#' @param tree data.tree
#' @param attrs character, optional. Vector of attributes to aggregate. Default
#' is all attributes.
#' @param aggFun function, optional. The function to be used to aggregate each
#' attribute. Default is \code{sum}.
#'
#' @return data.tree
#' @export
#'
Aggr_Attrs <- function(tree, attrs = NULL, aggFun = sum) {

  if (is.null(attrs)) {
    attrs <- tree$fieldsAll
  }

  purrr::walk(
    attrs,
    ~ tree$Do(
        .Aggr_Attr(attr   = .x,
                   aggFun = aggFun),
        traversal = 'post-order')
  )

  return(tree)

}

#' Aggregate Attribute
#'
#' @param attr character, name of the attribute to be aggregated up the tree
#' @param aggFun function, the function to be used for aggregation. Function
#' should take a numeric vector as its only argument and return a numeric singleton
#'
#' @return function
#'
#'
.Aggr_Attr <- function(attr, aggFun = sum) {

  function(x) {
    x[[attr]] <-
      Aggregate(node      = x,
                attribute = attr,
                aggFun    = aggFun)
  }

}

