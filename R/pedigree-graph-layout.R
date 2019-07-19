#' Pedigree graph layout
#'
#' @param funnels A data frame with funnels
#'
#' @return A dataframe
#' @export
#'
pedigree_graph_layout <- function(funnels) {

  ped <- funnel_to_pedigree(funnels, include_zero = TRUE)

  pedlg <- dplyr::group_by(dplyr::arrange(ped, gen, id), gen)

  pedlm <- dplyr::mutate(pedlg, xpos = (dplyr::row_number() - 1) / (dplyr::n() - 1))

  pedlnz <- pedlm[pedlm$gen != 0,]

  pedlg2 <- tidyr::gather(pedlnz, rel, parentid, f, m)

  pedlp <- dplyr::rename(pedlm, parentid = id, parentxpos = xpos, parentgen = gen)
  pedlout <- dplyr::left_join(pedlg2, pedlp)

  return(pedlout)
}
