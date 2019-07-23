
library(tidyverse)
library(data.tree)

# Testing Data ----

Make_Arbitrary_Hierarchy <- function(unit_names, max_depth = 1L, depth = NULL, min_subunits = NULL, max_subunits = NULL) {

  if (is.null(depth)) {
    depth <- 1L
  } else {
    depth <- depth + 1L
  }

  if (is.null(min_subunits)) {
    min_subunits <- 2L
  }

  if (is.null(max_subunits)) {
    max_subunits <- 9L
  }

  # Determine the number of subunits each unit will have
  if (min_subunits == max_subunits) {
    num_subunits <- rep(min_subunits, length(unit_names))
  } else {
    num_subunits <-
      sample.int(
        n = max_subunits - min_subunits,
        size = length(unit_names), replace = TRUE) + min_subunits - 1L
  }

  pathString <-
    purrr::map2(
      unit_names,
      num_subunits,
      ~ data.frame(
          parent = .x,
          # Force length to be equal to the depth by padding with zeros
          child = sprintf(
            sprintf('%%0%dd', depth),
            seq_len(.y))) %>%
        # Transform into a pathString
        dplyr::mutate(
          pathString = paste0(parent, '/', child)) %>%
        magrittr::extract2('pathString')
      ) %>%
    unlist()

  if (depth < max_depth) {
    # Recurse until max depth is reached
    pathString <-
      Make_Arbitrary_Hierarchy(
        unit_names = pathString,
        max_depth = max_depth,
        depth = depth,
        min_subunits = min_subunits,
        max_subunits = max_subunits)

  }

  pathString

}


# Function Definitions ----


# Script ----

# Makes an arbitrary hierarchy, then generates some random data as attributes
df_in <-
  data.frame(
    pathString     = Make_Arbitrary_Hierarchy(
      unit_names   = '0',
      max_depth    = 3L,
      min_subunits = 2L,
      max_subunits = 5L),
    stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    attr_A = runif(n = dplyr::n()),
    attr_B = runif(n = dplyr::n()),
    attr_C = runif(n = dplyr::n()),
    attr_D = runif(n = dplyr::n()),
    attr_E = runif(n = dplyr::n()),
    attr_F = runif(n = dplyr::n()),
    attr_G = runif(n = dplyr::n()),
    attr_H = runif(n = dplyr::n()),
    attr_I = runif(n = dplyr::n())
  )

# Inject NAs
df_in$attr_A[sample.int(n = nrow(df_in), size = 3L)] <- NA_real_
df_in$attr_B[sample.int(n = nrow(df_in), size = 3L)] <- NA_real_
df_in$attr_C[sample.int(n = nrow(df_in), size = 3L)] <- NA_real_
df_in$attr_D[sample.int(n = nrow(df_in), size = 3L)] <- NA_real_

tree <- df_in %>% data.tree::as.Node()

df_out_0 <-
  df_in %>%
  data.tree::as.Node() %>%
  Aggr_Attrs() %>%
  tree_to_df(minimal = FALSE)

metric_df <-
  df_out_0 %>%
  dplyr::transmute(
    pathString,
    level,
    metric_0.A.1 = (attr_A - attr_B) / attr_C,
    metric_0.A.2 = (attr_E / attr_I),
    metric_0.B.1 = (attr_D / attr_F) / ((attr_G - attr_H) / attr_I))

score_df <-
  metric_df %>%
  dplyr::group_by(level) %>%
  dplyr::mutate_at(
    dplyr::vars(dplyr::matches('^metric_')),
    scale) %>%
  dplyr::rename_at(
    dplyr::vars(dplyr::matches('^metric_')),
    ~ .x %>% stringr::str_replace('^metric_', 'scaled_metric_')) %>%
  dplyr::ungroup() %>%
  dplyr::select(-level)

df_out <-
  score_df %>%
  pivot_tree_df() %>%
  data.tree::as.Node(pathName = 'attr', pathDelimiter = '.') %>%
  Aggr_Attrs(aggFun = scaled_aggr) %>%
  tree_to_df(minimal = TRUE) %>%
  pivot_tree_df()
