#' @importFrom ggplot2 aes
ggplot2::aes

#' Make a pedigree graph
#'
#' @param funnels A dataframe with funnels
#' @param founder_names A character vector giving founder names in the same order as the funnels
#' @param highlight_funnel A string of length eight defining a funnel to highlight.
#' @param show_founder_id logical. Should the founder ids be shown?
#' @param show_points logical. Should a point be shown for each individual?
#'
#' @return a ggplot2 object
#' @export
pedigree_graph <- function(funnels, founder_names = NULL, highlight_funnel = NULL, show_founder_id = TRUE, show_points = TRUE) {
  if(!is.null(highlight_funnel)) {
    if(!inherits(highlight_funnel, "character") || nchar(highlight_funnel) != 8) stop("highlight_funnel should be a string with eight characters")
  }
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

  if(!is.null(highlight_funnel)) {
    cross1 <- unlist(strsplit(gsub("(.{2})", "\\1 ", highlight_funnel), " "))
    cross2 <- unlist(strsplit(gsub("(.{4})", "\\1 ", highlight_funnel), " "))

    highlight <- layout[layout$id %in% c(cross1, cross2, highlight_funnel),]

    p <- p +
      ggplot2::geom_segment(data = layout,
                            aes(x = xpos, y = gen, xend = parentxpos, yend = parentgen), colour = "grey") +
      ggplot2::geom_segment(data = highlight,
                            aes(x = xpos, y = gen, xend = parentxpos, yend = parentgen, colour = rel), size = 2)
  } else {
    p <- p +
      ggplot2::geom_segment(data = layout,
                            aes(x = xpos, y = gen, xend = parentxpos, yend = parentgen, colour = rel))
  }
  p <- p +
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
