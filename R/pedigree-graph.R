#' @importFrom ggplot2 aes
ggplot2::aes

#' Make a pedigree graph
#'
#' @param funnels A dataframe with funnels
#' @param founder_names A character vector giving founder names in the same order as the funnels
#' @param show_founder_id logical. Should the founder ids be shown?
#' @param show_points logical. Should a point be shown for each individual?
#'
#' @return a ggplot2 object
#' @export
pedigree_graph <- function(funnels, founder_names = NULL, show_founder_id = TRUE, show_points = TRUE) {
  layout <- pedigree_graph_layout(funnels)

  founders <- layout[layout$gen == 1, c("parentid", "parentxpos", "parentgen")]
  founders <- founders[match(unique(founders$parentxpos), founders$parentxpos),]

  prog <- dplyr::group_by(layout, gen, id)
  prog <- dplyr::filter(prog, dplyr::row_number() == 1)
  points <- data.frame(
    x = c(founders$parentxpos, prog$xpos),
    y = c(founders$parentgen, prog$gen)
  )
  p <- ggplot2::ggplot()

  if(show_points) {
    p <- p + geom_point(data = points, aes(x = x, y = y))
  }

  p <- p +
    ggplot2::geom_segment(data = layout,
                          aes(x = xpos, y = gen, xend = parentxpos, yend = parentgen, colour = rel)) +
    scale_color_discrete(name = "Relationship", labels = c("Female parent", "Male parent")) +
    ggplot2::scale_y_reverse() +
    theme_void() +
    theme(legend.position = "bottom")

  if(show_founder_id) {
    founders <- dplyr::arrange(founders, parentid)

    if(!is.null(founder_names)) {
      founders$names <- founder_names
    } else founders$names <- founders$parentid
    p <- p +
      geom_text(data = founders, aes(label = names, x = parentxpos, y = parentgen - 0.1))

  }

  return(p)
}
