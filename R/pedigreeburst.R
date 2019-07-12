#' @importFrom ggplot2 aes
ggplot2::aes

#' Pedigreeburst
#'
#' @param funnels A data.frame
#' @param print_founders Logical. Should founder names be printed in the figure?
#' @param show_legend Logical. Should a legend be printed?
#' @param
#'
#' @return a ggplot2 object
#' @export
#'
pedigreeburst <- function(funnels, print_founders = TRUE, show_legend = FALSE, rotate_labels = TRUE) {
  layout <- data_prep(funnels)

  fff <- dplyr::filter(layout, ymax == 1)

  founders <- dplyr::left_join(
    data.frame(id = 1:8,
               name = as.character(1:8),
               y = 0.5),
    fff)

  founders <- founders[!is.na(founders$set),]
  founders$x <- (founders$xmin + founders$xmax) / 2

  if(rotate_labels) {
    founders$theta <- (360 * (founders$x / max(founders$xmax)) - 90)
    founders$theta <- ifelse(founders$theta > 100 & founders$theta < 360, founders$theta - 180, founders$theta)
  } else {
    founders$theta <- 0
  }

  p <- ggplot2::ggplot(layout, aes(xmin = xmin, xmax = xmax,
                                   ymin = ymin, ymax = ymax + 0.01)) +
    ggplot2::geom_rect(aes(fill = as.factor(id))) +
    ggplot2::scale_fill_brewer(name = "Founders", palette = "Dark2") +
    ggplot2::ylim(c(-0.5, 4.4)) +
    ggplot2::coord_polar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank())

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
