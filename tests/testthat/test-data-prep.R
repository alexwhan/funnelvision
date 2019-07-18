
test_that("data-prep data checking works", {
  expect_silent(data_prep(funnels_right))
  expect_error(data_prep(funnels_wrong))
  expect_silent(data_prep(funnels_right, 2))
  expect_silent(data_prep(funnels_right, 7))
})
