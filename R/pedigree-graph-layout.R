#' Pedigree graph layout
#'
#' @param pedigree A pedigree dataframe with columns: f, m, id, gen
#'
#' @return A dataframe
#' @export
#'
pedigree_graph_layout <- function(pedigree) {

  pedlg <- dplyr::group_by(dplyr::arrange(pedigree, gen, id), gen)

  pedlm <- dplyr::mutate(pedlg, n = n(), xpos = (dplyr::row_number() - 1) / (dplyr::n() - 1))
  pedlm$xpos[pedlm$n == 1] <- 0.5
  pedlm <- pedlm[, c("f", "m", "id", "gen", "xpos")]

  pedlnz <- pedlm[pedlm$gen != 0,]

  pedlg <- tidyr::gather(pedlnz, rel, parentid, f, m)

  pedlp <- dplyr::rename(pedlm, parentid = id, parentxpos = xpos, parentgen = gen)
  pedlout <- dplyr::left_join(pedlg, pedlp)

  return(pedlout)
}
