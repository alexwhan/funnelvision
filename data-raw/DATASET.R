funnels_right <- data.frame(
  fff = c(1:8, 1, 7),
  ffm = c(2, 1, 4, 3, 6, 5, 8, 7, 2, 4),
  fmf = c(3, 4, 1, 2, 7, 8, 5, 6, 3, 8),
  fmm = c(4, 3, 2, 1, 8, 7, 6, 5, 4, 1),
  mff = c(5, 6, 7, 8, 4, 5, 1, 2, 5, 2),
  mfm = c(6, 5, 8, 7, 5, 4, 2, 1, 6, 6),
  mmf = c(7, 8, 5, 6, 1, 2, 3, 4, 7, 3),
  mmm = c(8, 7, 6, 5, 2, 1, 4, 3, 8, 5)

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
