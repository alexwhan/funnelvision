anim <- animate_funnel(funnels_right, "12345678")

test_that("animate-funnel works", {
  expect_true(inherits(anim, "gganim"))
})
