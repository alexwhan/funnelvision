

test_that("wrong funnel is detected", {
  expect_error(animate_layout(funnels_right, "15862374"))
})

test_that("animate_layout returns correctly", {
  expect_equal(nrow(animate_layout(funnels_right, "12345678")), 126)
})
