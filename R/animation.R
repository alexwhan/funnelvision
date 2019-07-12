#' @importFrom dplyr mutate
dplyr::mutate

#' @importFrom ggplot2 aes
ggplot2::aes

#' Animate a funnel in a pedigreeburst
#'
#' @param funnels A data.frame
#' @param funnel A string describing the funnel of interest
#' @param show_base Logical. Should the base pedigree be shown?
#' @param show_animation Logical. Should the animation be shown?
#'
#' @return
#' @export
#'
#' @examples
animate_funnel <- function(funnels, funnel, show_base = TRUE, show_animation = TRUE) {
  if(!require(gganimate)) stop("The gganimate package is required for this function")

  dat <- data_prep(funnels)

  select_funnel <- funnel

  founder_funnel <- unlist(strsplit(funnel, ""))

  cross1_funnel <- unlist(strsplit(gsub("(.{2})", "\\1 ", funnel), " "))

  cross2_funnel <- unlist(strsplit(gsub("(.{4})", "\\1 ", funnel), " "))

  animdat <- dplyr::bind_rows(
    mutate(dplyr::filter(dat,
                         cross1 == cross1_funnel[1] & level %in% 1:2),
           state = 2),
    mutate(dplyr::filter(dat,
                         (cross1 == cross1_funnel[1] & level %in% 1:2) |
                           (cross1 == cross1_funnel[2] & level %in% 1:2)),
           state = 3),
    mutate(dplyr::filter(dat,
                         cross2 == cross2_funnel & level %in% 1:4),
           state = 4),
    mutate(dplyr::filter(dat,
                         (cross2 == cross2_funnel & level %in% 1:4) |
                           (cross1 == cross1_funnel[3] & level %in% 1:2)),
           state = 6),
    mutate(dplyr::filter(dat,
                         (cross2 == cross2_funnel & level %in% 1:4) |
                           (cross1 == cross1_funnel[3] & level %in% 1:2)|
                           (cross1 == cross1_funnel[4] & level %in% 1:2)),
           state = 8),
    mutate(dplyr::filter(dat,
                         (cross2 == cross2_funnel & level %in% 1:4) |
                           (cross2 == cross2_funnel[2] & level %in% 1:4)),
           state = 9),
    mutate(dplyr::filter(dat,
                         funnel == select_funnel),
           state = 10),
    mutate(dat, state = 11)
  )

  animp <- ggplot2::ggplot(animdat, aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax + 0.01)) +
    ggplot2::geom_rect(data = dat, fill = "grey", colour = "darkgrey") +
    ggplot2::geom_rect(aes(fill = as.factor(id))) +
    ggplot2::xlim(c(0, 8)) +
    ggplot2::labs(title = "State: {closest_state}") +
    ggplot2::scale_fill_brewer(name = "Founders", palette = "Dark2") +
    ggplot2::ylim(c(-0.5, 4.4)) +
    ggplot2::coord_polar() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none") +
    ggaminate::transition_states(state) +
    gganimate::enter_fade() +
    gganimate::exit_shrink() +
    gganimate::ease_aes()

  if(show_animation) gganimate::animate(animp, rewind = FALSE)

  return(animp)
}

