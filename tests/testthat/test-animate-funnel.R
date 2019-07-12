funnels_right <- data.frame(
  fff = c(1:8),
  mff = c(2, 1, 4, 3, 6, 5, 8, 7),
  fmf = c(3, 4, 1, 2, 7, 8, 5, 6),
  mmf = c(4, 3, 2, 1, 8, 7, 6, 5),
  ffm = c(5, 6, 7, 8, 4, 5, 1, 2),
  mfm = c(6, 5, 8, 7, 5, 4, 2, 1),
  fmm = c(7, 8, 5, 6, 1, 2, 3, 4),
  mmm = c(8, 7, 6, 5, 2, 1, 4, 3)
)

anim <- animate_funnel(funnels_right, "12345678")

test_that("animate-funnel works", {
  expect_silent(animate_funnel(funnels_right, "12345678"))
  expect_true(inherits(anim, "gganim"))
})
