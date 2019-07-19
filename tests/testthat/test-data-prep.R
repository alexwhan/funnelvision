
test_that("data-prep data checking works", {
  expect_silent(funnel_layout(funnels_right))
  expect_error(funnel_layout(funnels_wrong))
  expect_silent(funnel_layout(funnels_right, 2))
  expect_silent(funnel_layout(funnels_right, 7))
})
