test_that('Make Arbitrary Hierarchy works with lowercase alternate', {
  expect_equal(
    c('0/0a/0a1/0a1a'),
    .Make_Arbitrary_Hierarchy(
      max_depth = 3L,
      child_opts = 1L,
      name_mode = 'alternate'))
})

test_that("Child name maker works", {
  expect_equal(
    c('0/0A/0A1','0/0A/0A2','0/0B/0B1', '0/0B/0B2'),
    .Make_Arbitrary_Hierarchy(max_depth = 2L, child_opts = 2L, name_mode = 'ALTERNATE'))
})
