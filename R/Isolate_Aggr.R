
#' Isolate and Aggregate
#'
#' This function is used to make a list of trees, one for each unique value in
#' the isolate column in the data.frame \code{df}
#'
#' @inheritParams Make_Isolate_Tree
#' @inheritParams Aggr_Attrs
#'
#' @return data.frame
#' @export
#'
Isolate_Aggr <- function(df, isolate, over, attrs = NULL, aggFun = sum) {

  # TODO: Make it possible to isolate an arbitrary number of columns.
  # EnQuose arguments
  isolate <- rlang::enquo(isolate)
  over <- rlang::enquo(over)

  isolate_paths <-
    df %>%
      dplyr::select(!!isolate) %>%
      dplyr::pull() %>%
      unique()

  isolate_paths %>%
    purrr::map_dfr(
      ~ .Isolate_Aggr(
          df = df,
          isolate_path = .x,
          isolate = !!isolate,
          over    = !!over,
          attrs   = attrs,
          aggFun  = aggFun))

}

#' Isolate and Aggregate (Helper)
#'
#' @inheritParams Make_Isolate_Tree
#' @inheritParams Aggr_Attrs
#'
#' @return data.frame
#' @export
#'
.Isolate_Aggr <- function(df, isolate_path, isolate, over, attrs = NULL, aggFun = sum) {

  isolate <- rlang::enquo(isolate)
  over <- rlang::enquo(over)

  Make_Isolate_Tree(
    df = df,
    isolate_path = isolate_path,
    isolate = !!isolate,
    over    = !!over) %>%
  Aggr_Attrs(
    attrs  = attrs,
    aggFun = aggFun) %>%
  Make_Aggd_Df(
    isolate_path = isolate_path,
    isolate      = !!isolate,
    over         = !!over)

}

#' Make Isolate Tree
#'
#' @param df data.frame
#' @param isolate_path character, value of isolate that will be isolated
#' @param isolate symbol, a pathString (or other) field which will be held
#' constant
#' @param over symbol, a pathString field which will be aggregated according to
#' hierarchy
#' @param ... symbols, columns to be retained as attributes in the resulting
#' data.tree. If missing, all other columns will be retained as attributes.
#'
#' @return data.tree
#' @export
#'
#' @import rlang
#' @importFrom dplyr filter select
#'
Make_Isolate_Tree <- function(df, isolate_path, isolate, over, ...) {

  # enQuote inputs, because we will be using Non-Standard Evaluation (NSE)
  isolate <- rlang::enquo(isolate)
  over <- rlang::enquo(over)
  aggr <- rlang::enquos(...)

  qn_isolate <- rlang::quo_name(isolate)
  qn_over <- rlang::quo_name(over)

  # If aggr columns have not been supplied, get all columns not being isolated
  # or aggregated over
  if (length(aggr) == 0L) {
    aggr <- rlang::syms(names(df)[!names(df) %in% c(qn_isolate, qn_over)])
  }

  #   1. Take the data.frame and filter it to only the the rows where the isolate
  #       column is equal to the string isolate_path.
  #   2. Select the "over" column and any columns that we want in the data.tree,
  #       which keeps extraneous columns out of our data.tree.
  #   3. Turn the result of the above steps into a data.tree, using the "over"
  #       column as the pathName, which is then used for hierarchical
  #       aggregation.
  df %>%
    dplyr::filter(!!isolate == isolate_path) %>%
    dplyr::select(
      !!over,
      !!!aggr) %>%
    data.tree::as.Node(pathName = qn_over)
}

#' Make Aggregated Dataframe
#'
#' @param aggd_tree data.tree, most often the result returned by \code{Aggr_Attrs()}.
#' @inheritParams Make_Isolate_Tree
#'
#' @return data.frame
#' @export
#'
#' @import rlang
#' @importFrom dplyr rename mutate select left_join
#'
Make_Aggd_Df <- function(aggd_tree, isolate_path, over, isolate) {

  # EnQuose over and isolate arguments, because we expect symbols
  over <- rlang::enquo(over)
  isolate <- rlang::enquo(isolate)

  # 1. Take the aggregated data.tree and transform it into a data.frame
  # 2. The hierarchy column we aggregated over will be called "pathString", so
  #     we need to rename it to turn it back into what it was originally.
  # 3. The isolate column will be missing entirely, and so we have to add it
  #     back into the data.frame, using the value isolate_path.

  aggd_tree %>%
    aggrtree::tree_to_df() %>%
    dplyr::rename(
      !!over := pathString) %>%
    dplyr::mutate(
      !!isolate := isolate_path) %>%
    dplyr::select(!!isolate, !!over, dplyr::everything())

}
