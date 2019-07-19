#' @importFrom dplyr mutate
dplyr::mutate

#' Animate a funnel in a funnelburst
#'
#' @param funnels A data.frame
#' @param funnel A string describing the funnel of interest
#'
#' @return A gganim object
#' @export
#'
animate_layout <- function(funnels, funnel) {
  if(!require(gganimate)) stop("The gganimate package is required for this function")

  dat <- data_prep(funnels)

  if(!funnel %in% dat$funnel) stop(paste("Funnel", funnel, "is not in the funnels dataframe"))

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
                         cross2 %in% cross2_funnel & level %in% 1:4),
           state = 3),
    mutate(dplyr::filter(dat,
                         (cross2 %in% cross2_funnel & level %in% 1:4) |
                           (cross1 == cross1_funnel[3] & level %in% 1:2)),
           state = 4),
    mutate(dplyr::filter(dat,
                         (cross2 %in% cross2_funnel & level %in% 1:4) |
                           (cross1 == cross1_funnel[3] & level %in% 1:2)|
                           (cross1 == cross1_funnel[4] & level %in% 1:2)),
           state = 5),
    mutate(dplyr::filter(dat,
                         (cross2 %in% cross2_funnel & level %in% 1:4) |
                           (cross2 == cross2_funnel[2] & level %in% 1:4)),
           state = 6),
    mutate(dplyr::filter(dat,
                         funnel == select_funnel),
           state = 7),
    mutate(dat, state = 8)
  )

  return(animdat)

}

