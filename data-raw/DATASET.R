funnels_right <- data.frame(
  fff = c(1:8),
  ffm = c(2, 1, 4, 3, 6, 5, 8, 7),
  fmf = c(3, 4, 1, 2, 7, 8, 5, 6),
  fmm = c(4, 3, 2, 1, 8, 7, 6, 5),
  mff = c(5, 6, 7, 8, 4, 5, 1, 2),
  mfm = c(6, 5, 8, 7, 5, 4, 2, 1),
  mmf = c(7, 8, 5, 6, 1, 2, 3, 4),
  mmm = c(8, 7, 6, 5, 2, 1, 4, 3)
)

usethis::use_data(funnels_right, overwrite = TRUE)

funnels_wrong <- data.frame(
  fff = c(1, 2),
  ffm = c(2, 1),
  fmf = c(3, 4),
  fmm = c(4, 3),
  mff = c(5, 6),
  mfm = c(6, 5),
  mmf = c(7, 8),
  mm = c(8, 7)
)

usethis::use_data(funnels_wrong, overwrite = TRUE)
