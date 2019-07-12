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
#' @return A gganim object
#' @export
#'
animate_funnel <- function(funnels, funnel, show_base = TRUE, show_animation = FALSE) {
  if(!require(gganimate)) stop("The gganimate package is required for this function")

  dat <- data_prep(funnels)

  dat$cross1 <- gsub("(^.{2}).*", "\\1", dat$funnel)

  dat$cross2 <- gsub("(^.{4}).*", "\\1", dat$funnel)

  select_funnel <- funnel

  founder_funnel <- unlist(strsplit(funnel, ""))

  cross1_funnel <- unlist(strsplit(gsub("(.{2})", "\\1 ", funnel), " "))

  cross2_funnel <- unlist(strsplit(gsub("(.{4})", "\\1 ", funnel), " "))

  animdat <- dplyr::bind_rows(
    mutate(dplyr::filter(dat,
                         cross1 == cross1_funnel[1] & level %in% 1:2),
           state = 1),
    mutate(dplyr::filter(dat,
                         (cross1 == cross1_funnel[1] & level %in% 1:2) |
                           (cross1 == cross1_funnel[2] & level %in% 1:2)),
           state = 2),
    mutate(dplyr::filter(dat,
                         cross2 == cross2_funnel & level %in% 1:4),
           state = 3),
    mutate(dplyr::filter(dat,
                         (cross2 == cross2_funnel & level %in% 1:4) |
                           (cross1 == cross1_funnel[3] & level %in% 1:2)),
           state = 4),
    mutate(dplyr::filter(dat,
                         (cross2 == cross2_funnel & level %in% 1:4) |
                           (cross1 == cross1_funnel[3] & level %in% 1:2)|
                           (cross1 == cross1_funnel[4] & level %in% 1:2)),
           state = 5),
    mutate(dplyr::filter(dat,
                         (cross2 == cross2_funnel & level %in% 1:4) |
                           (cross2 == cross2_funnel[2] & level %in% 1:4)),
           state = 6),
    mutate(dplyr::filter(dat,
                         funnel == select_funnel),
           state = 7),
    mutate(dat, state = 8)
  )


  if(show_base) {
    base <- pedigreeburst(funnels, print_founders = FALSE, colour_by_founder = FALSE)
  }

  animp <- base +
    ggplot2::geom_rect(data = animdat,
                       aes(fill = as.factor(id))) +
    ggplot2::labs(title = "State: {closest_state}") +
    ggplot2::scale_fill_brewer(name = "Founders", palette = "Dark2") +
    gganimate::transition_states(state) +
    gganimate::enter_fade() +
    gganimate::exit_shrink() +
    gganimate::ease_aes()

  if(show_animation) gganimate::animate(animp, rewind = FALSE)

  return(animp)
}

