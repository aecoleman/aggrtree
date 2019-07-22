#' Aggregate Scaled Values
#'
#' Aggregates values that have been scaled by averaging them and then
#' multiplying by the square root of n
#'
#' @param x numeric, scaled values to be aggregated
#'
#' @return
#' @export
#'
Aggr_Scaled <- function(x) {

  x <- x[!is.na(x)]

  if (length(x) > 0L) {
    mean(x) * sqrt(length(x))
  } else {
    NA_real_
  }

}
