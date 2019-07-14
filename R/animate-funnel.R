#' @importFrom ggplot2 aes
ggplot2::aes

#' @param funnels A data.frame
#' @param funnel A string describing the funnel of interest
#' @param states numeric vector describing which states should be included in the animation
#' @param show_base Logical. Should the base pedigree be shown?
#' @param show_animation Logical. Should the animation be shown?

animate_funnel <- function(funnels, funnel, states = 1:8, nframes = 100, show_base = TRUE, show_animation = FALSE) {

  animdat <- animate_layout(funnels, funnel)

  animdat <- animdat[animdat$state %in% states,]

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

  if(show_animation) gganimate::animate(animp, rewind = FALSE, nframes = nframes)

  return(animp)
}
