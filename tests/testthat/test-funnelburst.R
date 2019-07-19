test_that("funnelburst argument checks work", {
  expect_error(funnelburst(funnels_right, "12345678"))
  expect_error(funnelburst(funnels_right, show_levels = 2:5))
  expect_error(funnelburst(funnels_right, show_levels = TRUE))
})

test_that("funnelburst returns ggplot object", {
  expect_true(inherits(funnelburst(funnels_right), "ggplot"))
})
