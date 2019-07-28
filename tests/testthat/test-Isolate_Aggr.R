# Test Setup ###################################################################

library(magrittr)

# set.seed(2)

isolate_vars <- c('0/A', '0/B', '0/C')
over_vars <- c('0/x/xa', '0/x/xb', '0/y/ya', '0/y/yb', '0/z')

df <- expand.grid(
  isolate_var = isolate_vars,
  over_var    = over_vars,
  stringsAsFactors = FALSE)

df$measure_1 <- rbinom(n = nrow(df), size = 10L, prob = runif(n = 1))
df$measure_2 <- runif(n = nrow(df)) * 10
df$measure_3 <- runif(n = nrow(df)) - 0.5
df$measure_D <- sample(letters, size = nrow(df), replace = TRUE)

isolate_path <- sample(isolate_vars, size = 1)

# Test 0: No arguments passed in  ... ##########################################
# Expected Outcome:
#     Should produce a tree that contains one root node, 0, with three children,
#   0/X, 0/Y, and 0/Z. Each child node should have all 4 attributes, named
#   measure_1, measure_2, measure_3, and measure_D.

dtree_0_arg <-
  df %>%
  dplyr::filter(isolate_var == isolate_path) %>%
  dplyr::select(-isolate_var) %>%
  data.tree::as.Node(pathName = 'over_var')

test_that("Make_Isolate_Tree works with no arguments in '...'", {
  expect_equal(
    dtree_0_arg,
    Make_Isolate_Tree(
      df = df,
      isolate_path = isolate_path,
      isolate = isolate_var,
      over = over_var))
})

# Test 1: One argument passed in ... ###########################################
# Expected Outcome:
#     Should produce a tree that contains one root node, 0, with three children,
#   0/X, 0/Y, and 0/Z. Each child node should have one attribute. The name of
#   this attribute will be equal to the value of the variable "measure"

measure <-
  names(df) %>%
  stringr::str_subset('^measure_[0-9]+') %>%
  sample(size = 1L) %>%
  rlang::sym()

dtree_1_arg <-
  df %>%
  dplyr::filter(isolate_var == isolate_path) %>%
  dplyr::select(-isolate_var) %>%
  dplyr::select(over_var, !!measure) %>%
  data.tree::as.Node(pathName = 'over_var')

test_that("Make_Isolate_Tree works with one argument in '...'", {
  expect_equal(
    dtree_1_arg,
    Make_Isolate_Tree(
      df = df,
      isolate_path = isolate_path,
      isolate      = isolate_var,
      over         = over_var,
      !!measure))
})

# Test 2: Two arguments passed in ... ##########################################
# Expected Outcome:
#     Should produce a tree that contains one root node, 0, with three children,
#   0/X, 0/Y, and 0/Z. Each child node should have two attributes. The names of
#   the attributes will be equal to the values of the variable "measures", and
#   should appear in the same order as they do in that variable.

measures <-
  names(df) %>%
  stringr::str_subset('^measure_[0-9]+') %>%
  sample(size = 2L) %>%
  rlang::syms()

dtree_2_arg <-
  df %>%
  dplyr::filter(isolate_var == isolate_path) %>%
  dplyr::select(-isolate_var) %>%
  dplyr::select(over_var, !!!measures) %>%
  data.tree::as.Node(pathName = 'over_var')

test_that("Make_Isolate_Tree works with more than one argument in '...'", {
  expect_equal(
    dtree_2_arg,
    Make_Isolate_Tree(
      df = df,
      isolate_path = isolate_path,
      isolate = isolate_var,
      over = over_var,
      !!!measures))
})

test_that('Isolate_Aggr works as expected', {
  expect_equal(
    df %>%
      dplyr::group_by(isolate_var) %>%
      dplyr::summarize_at(
        dplyr::vars(dplyr::matches('^measure_[0-9]+$')), sum) %>%
      dplyr::mutate(over_var = '0'),

    df %>%
      dplyr::select(-measure_D) %>%
      Isolate_Aggr(
        isolate = isolate_var,
        over    = over_var,
        aggFun  = sum) %>%
      dplyr::filter(over_var == '0'))
})
