test_that("pedigreeburst argument checks work", {
  expect_error(pedigreeburst(funnels_right, "12345678"))
  expect_error(pedigreeburst(funnels_right, show_levels = 2:5))
  expect_error(pedigreeburst(funnels_right, show_levels = TRUE))
})

test_that("pedigreeburst returns ggplot object", {
  expect_true(inherits(pedigreeburst(funnels_right), "ggplot"))
})
