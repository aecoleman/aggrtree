#' Aggregate Attributes
#'
#' @param node data.tree Node object
#' @param attrs character, optional. Vector of attributes to aggregate. Default
#' is all attributes.
#' @param aggFun function, optional. The function to be used to aggregate each
#' attribute. Default is \code{sum}.
#'
#' @return data.tree
#' @export
#'
#' @importFrom purrr walk
#'
Aggr_Attrs <- function(node, attrs = NULL, aggFun = sum) {

  stopifnot(is.Node(node))

  if (is.null(attrs)) {
    attrs <- node$fieldsAll
  }

  purrr::walk(
    attrs,
    ~ node$Do(
        .Aggr_Attr(attr   = .x,
                   aggFun = aggFun),
        traversal = 'post-order')
  )

  return(node)

}

#' Aggregate Attribute
#'
#' @param attr character, name of the attribute to be aggregated up the tree
#' @param aggFun function, the function to be used for aggregation. Function
#' should take a numeric vector as its only argument and return a numeric singleton
#'
#' @return function
#'
#' @importFrom data.tree Aggregate
#'
.Aggr_Attr <- function(attr, aggFun = sum) {

  function(x) {
    x[[attr]] <-
      data.tree::Aggregate(node      = x,
                attribute = attr,
                aggFun    = aggFun)
  }

}

