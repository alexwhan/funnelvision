
funnel <- "12345678"
funnel_sep <- unlist(strsplit(funnel, ""))
#first cross

dat <- pedigreeburst:::data_prep(funnels_right) %>%
  mutate(cross1 = gsub("(^.{2}).*", "\\1", funnel),
         cross2 = gsub("(^.{4}).*", "\\1", funnel)) %>%
  arrange(ymax) %>%
  group_by(ymax) %>%
  mutate(level = group_indices())

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
  # mutate(dplyr::filter(dat,
  #                      (cross2 == cross2_funnel & level %in% 1:4) |
  #                      (as.character(id) %in% founder_funnel[5:6] & level == 1)),
  #        state = 5),
  mutate(dplyr::filter(dat,
                       (cross2 == cross2_funnel & level %in% 1:4) |
                         (cross1 == cross1_funnel[3] & level %in% 1:2)),
         state = 6),
  # mutate(dplyr::filter(dat,
  #                      (cross2 == cross2_funnel & level %in% 1:4) |
  #                        (cross1 == cross1_funnel[3] & level %in% 1:2)|
  #                        (as.character(id) %in% founder_funnel[7:8] & level == 1)),
  #        state = 7),
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
  geom_rect(data = dat, fill = "grey", colour = "darkgrey") +
  ggplot2::geom_rect(aes(fill = as.factor(id))) +
  xlim(c(0, 8)) +
  labs(title = "State: {closest_state}") +
  ggplot2::scale_fill_brewer(name = "Founders", palette = "Dark2") +
  ggplot2::ylim(c(-0.5, 4.4)) +
  ggplot2::coord_polar() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = element_blank(),
                 axis.title = element_blank(),
                 panel.grid = element_blank()) +
  ggplot2::theme(legend.position = "none") +
  transition_states(state) +
  enter_fade() +
  exit_shrink() +
  ease_aes()

animate(animp, rewind = FALSE)
