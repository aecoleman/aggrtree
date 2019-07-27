# Test Setup ###################################################################

df <- aggrtree:::.Make_Test_DF()

isotrees <-
  df$isolate_var %>%
  unique() %>%
  purrr::map(
    ~ Make_Isolate_Tree(
        df = df,
        isolate_path = .x,
        isolate = isolate_var,
        over    = over_var))


testthat::test_that("Make_Isolate_Trees works", {

  expect_equal(
    isotrees,
    Make_Isolate_Trees(
      df = df,
      isolate = isolate_var,
      over = over_var))

})