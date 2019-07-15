#' Funnel to pedigree
#'
#' @param funnels A dataframe describing funnels
#' @param include_zero Logical. Whether to include the zero (non-crossing) generation
#'
#' @return
#' @export
funnel_to_pedigree <- function(funnels, include_zero = FALSE) {

  fdf <- funnels


  f1 <- c(fdf$fff, fdf$fmf, fdf$ffm, fdf$fmm)
  m1 <- c(fdf$mff, fdf$mmf, fdf$mfm, fdf$mmm)
  id1 <- paste0(f1, m1)

  f0 <- NULL
  m0 <- NULL
  id0 <- NULL
  gen0 <- NULL

  if(include_zero) {
    id0 <- unique(c(f1, m1))
    f0 <- rep(0, length(id0))
    m0 <- rep(0, length(id0))
    gen0 <- rep(0, length(id0))
  }


  f2 <- c(paste0(fdf$fff, fdf$mff), paste0(fdf$ffm, fdf$mfm))
  m2 <- c(paste0(fdf$fmf, fdf$mmf), paste0(fdf$fmm, fdf$mmm))
  id2 <- paste0(f2, m2)

  f3 <- paste0(fdf$fff, fdf$mff, fdf$fmf, fdf$mmf)
  m3 <- paste0(fdf$ffm, fdf$mfm, fdf$fmm, fdf$mmm)
  id3 <- paste0(f3, m3)

  ped <-  bind_rows(
    tibble::tibble(f = c(f0, f1, f2, f3),
           m = c(m0, m1, m2, m3),
           id = c(id0, id1, id2, id3),
           gen = c(gen0,
                   rep(1, times = nrow(fdf) * 4),
                   rep(2, times = nrow(fdf) * 2),
                   rep(3, times = nrow(fdf))))
  )

  return(ped)
}
