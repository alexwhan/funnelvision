#' @importFrom ggplot2 aes
ggplot2::aes

#' Pedigreeburst
#'
#' @param funnels A data.frame
#' @param show_levels A numeric vector describing which levels to show in the figure. Must start at 1
#' @param print_founders Logical. Should founder names be printed in the figure?
#' @param show_legend Logical. Should a legend be printed?
#' @param rotate_labels = TRUE
#' @param xmax numeric. Controls limits if a subset of funnels are used
#' @param colour_by_founder Logical. Default TRUE. If FALSE a grey figure is returned
#' @param padding numeric. Controls spacing between crossing levels
#'
#' @return a ggplot2 object
#' @export
#'
pedigreeburst <- function(funnels, focus_level = 1, show_levels = 1:8, print_founders = TRUE, show_legend = FALSE, rotate_labels = TRUE, xmax = NULL, colour_by_founder = TRUE, padding = 0.1) {

  if(!is.integer(show_levels)) stop("show_levels must be an integer vector")

  if(min(show_levels) != 1) stop("show_levels must start at 1")

  if(!is.logical(print_founders)) stop("print_founders must be logical")

  layout <- data_prep(funnels, focus_level)

  if(max(show_levels) > max(layout$level)) stop("show_levels has a higher maximum than are present in the data")

  layout <- layout[layout$level <= max(show_levels),]

  fff <- dplyr::filter(layout, level == focus_level)


  founders <- dplyr::left_join(
    data.frame(id = 1:8,
               name = as.character(1:8)),
    fff)
  founders$y <- mean(c(founders$ymin, founders$ymax), na.rm = TRUE)

  founders <- founders[!is.na(founders$set),]
  founders$x <- (founders$xmin + founders$xmax) / 2

  founders_g <- dplyr::group_by(founders, id)
  founders <- dplyr::summarise(founders_g, x = mean(x), y = mean(y), name = name[1], xmax = max(xmax), xmin = min(xmin), ymin = min(ymin), ymax = max(ymax))

  if(rotate_labels) {
    founders$theta <- (360 * (founders$x / max(founders$xmax)) - 90)
    founders$theta <- ifelse(founders$theta > 100 & founders$theta < 360, founders$theta - 180, founders$theta)
  } else {
    founders$theta <- 0
  }

  p <- ggplot2::ggplot(layout, aes(xmin = xmin, xmax = xmax,
                                   ymin = ymin, ymax = ymax + 0.01))

  if(!colour_by_founder) {
    p <- p + ggplot2::geom_rect(fill = "grey", colour = "darkgrey")
  } else {
    p <- p + ggplot2::geom_rect(aes(fill = as.factor(id))) +
      ggplot2::scale_fill_brewer(name = "Founders", palette = "Dark2")
  }

  p <- p + ggplot2::ylim(c(-1.5, max(layout$ymax) * 1.1)) +
    ggplot2::coord_polar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())

  if(print_founders) {
    p <- p +
      ggplot2::geom_text(data = founders, aes(x = x, y = y, label = name, angle = -theta),
                         colour = "white")
  }

  if(!show_legend) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}
