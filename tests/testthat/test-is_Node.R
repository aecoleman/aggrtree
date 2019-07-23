test_that("is.Node sensitivity", {
  expect_equal(TRUE, is.Node(data.tree::Node$new('proto')))
})

test_that("is.Node sensitivity vector", {
  expect_equal(
    c(TRUE, TRUE, TRUE),
    is.Node(
      list(data.tree::Node$new('proto'),
           data.tree::Node$new('proto'),
           data.tree::Node$new('proto'))))
})

test_that('is.Node specificity', {
  expect_equal(FALSE, is.Node('a'))
})

test_that('is.Node specificity list', {
  expect_equal(c(FALSE, FALSE, FALSE), is.Node(list('a', 'b', 'c')))
})

test_that('is.Node mix list', {
  expect_equal(
    c(FALSE, TRUE, FALSE),
    is.Node(
      list('a',
           data.tree::Node$new('proto'),
           'c')))
})
