#' @importFrom rlang .data
rlang::.data

#' @importFrom dplyr case_when
dplyr::case_when


#' Prepare data for pedigree burst
#'
#' @param funnels A data.frame
#' @param padding Numeric. How much space between crosses. Default 0.1
#'
#' @return A data.frame
#'
data_prep <- function(funnels, focus_level = 1, padding = 0.1) {
  if(!inherits(funnels, "data.frame")) stop(paste(funnels, "is not a data.frame"))

  if(!all(colnames(funnels) %in% c("fff", "mff", "fmf", "mmf", "ffm", "mfm", "fmm", "mmm"))) {
    stop('The funnels dataframe must have columns names in the form: "fff", "mff", "fmf" "mmf", "ffm", "mfm", "fmm", "mmm"')
  }

  n <- focus_level
  n2 <- ifelse(n %% 2 == 0, n - 1, n + 1)
  n3 <- ifelse(n > 4,
               ifelse(n > 6,
                      (n - 1) %% 2 + 5,
                      (n - 1) %% 2 + 7),
               ifelse(n > 2,
                      (n - 1) %% 2 + 1,
                      (n - 1) %% 2 + 3))
  n4 <- ifelse(n3 %% 2 == 0, n3 - 1, n3 + 1)
  n5 <- (n + 3) %% 8 + 1
  n6 <- ifelse(n5 %% 2 == 0, n5 - 1, n5 + 1)
  n7 <- ifelse(n5 > 4,
               ifelse(n5 > 6,
                      (n5 - 1) %% 2 + 5,
                      (n5 - 1) %% 2 + 7),
               ifelse(n5 > 2,
                      (n5 - 1) %% 2 + 1,
                      (n5 - 1) %% 2 + 3))
  n8 <- ifelse(n7 %% 2 == 0, n7 - 1, n7 + 1)

  nvec <- c(n, n2, n3, n4, n5, n6, n7, n8)
  if(!all(nvec[order(nvec)] == 1:8)) stop("Something went wrong with the layer ordering")
  if(!all(unlist(lapply(funnels, is.numeric)))) stop("There are non-numeric columns in the input data.frame")

  fs <- funnels[, c("fff", "ffm", "fmf", "fmm", "mff", "mfm", "mmf", "mmm")]

  #We need to have the rows sorted according to the focus level, because that is how the geometry gets determined
  funnels_sort <- fs[order(fs[[n]], fs[[n2]], fs[[n3]], fs[[n4]], fs[[n5]], fs[[n6]], fs[[n7]], fs[[n8]]),]

  #This calculates a min and max position for each RIL. So this will ensure every RIL has equal spacing in the outer ring,
  # and will allow the inner rings to be (progressively) determined
  funnels_n <- dplyr::mutate(funnels_sort, xmin = 0:(dplyr::n() - 1),
                             xmax = 1:dplyr::n(),
                             sort_order = 1:dplyr::n(),
                             funnel = paste0(fff, ffm, fmf, fmm, mff, mfm, mmf, mmm))

  # This chunk calculates the outer ring. These are the founders that are on the paternal side for the final cross (hence "^m")
  funnels_outer_raw <- dplyr::select(funnels_n, dplyr::matches("^m"), xmin, xmax, funnel)

  funnels_outer_long <- tidyr::gather(funnels_outer_raw, level, id, -xmin, -xmax, -funnel)

  # getting the y (distance from centre)
  funnels_outer_y <- dplyr::mutate(funnels_outer_long,
                                   level = factor(level, levels = c("mff", "mfm", "mmf", "mmm")),
                                   level_num = as.numeric(level),
                                   ymin = case_when(
                                     level_num < 3 ~ level_num + 3 + 5 * padding,
                                     TRUE ~ level_num + 3 + 6 * padding),
                                   ymax = case_when(
                                     level_num < 3 ~ level_num + 4.1 + 4 * padding,
                                     TRUE ~ level_num + 4.1 + 5 * padding))

  funnels_outer <- dplyr::select(funnels_outer_y, id, xmin, xmax, ymin, ymax, funnel, level_num)

  # calculate the middle ring

  # because we collapse rils into segments for this ring, we need to know the min and the max for each funnel at the middle stage
  funnels_middle_raw <- dplyr::select(funnels_n, fff, ffm, fmf, fmm, xmin, xmax, funnel)

  funnels_middle_group <- dplyr::group_by(funnels_middle_raw, fff, ffm, fmf, fmm)

  funnels_middle_sum <- dplyr::summarise(funnels_middle_group,
                                         xmin = min(xmin),
                                         xmax = max(xmax),
                                         funnel = funnel[1])
  funnels_middle_clean <- dplyr::select(dplyr::ungroup(funnels_middle_sum), fmf, fmm, xmin, xmax, funnel)

  funnels_middle_long <- tidyr::gather(funnels_middle_clean, level, id, -xmin, -xmax, -funnel)

  funnels_middle_m <- dplyr::mutate(funnels_middle_long,
                                  level = factor(level, levels = c("fmf", "fmm")),
                                  level_num = as.numeric(level),
                                  ymin = level_num + 1 + padding,
                                  ymax = level_num + 2.1 + padding)

  funnels_middle <- dplyr::select(funnels_middle_m, id, xmin, xmax, ymin, ymax, funnel)

  #calculate the central paternal ring

  funnels_cp_raw <- dplyr::select(funnels_n, fff, ffm, xmin, xmax, funnel)

  funnels_cp_g <- dplyr::group_by(funnels_cp_raw, fff, ffm)

  funnels_cp_sum <- dplyr::ungroup(dplyr::summarise(funnels_cp_g,
                                                    xmin = min(xmin),
                                                    xmax = max(xmax),
                                                    funnel = funnel[1]))
  funnels_cp_select <- dplyr::select(funnels_cp_sum, id = ffm, xmin, xmax, funnel)

  funnels_cp <- dplyr::mutate(funnels_cp_select,
                              ymin = 1,
                              ymax = 2,
                              id = id)

  #central maternal ring
  funnels_cm_raw <- dplyr::select(funnels_n, fff, xmin, xmax, funnel)

  funnels_cm_g <- dplyr::group_by(funnels_cm_raw, fff)

  funnels_cm_sum <- dplyr::ungroup(dplyr::summarise(funnels_cm_g,
                                                    xmin = min(xmin),
                                                    xmax = max(xmax),
                                                    funnel = funnel[1]))

  funnels_cm_select <- dplyr::select(funnels_cm_sum,
                                     id = fff,
                                     xmin,
                                     xmax,
                                     funnel)

  funnels_cm <- dplyr::mutate(funnels_cm_select,
                              ymin = 0,
                              ymax = 1.1,
                              id = id)

  all_layout <- dplyr::bind_rows(funnels_outer,
                                 funnels_middle,
                                 funnels_cp,
                                 funnels_cm)


  all_layout_m <- dplyr::mutate(all_layout,
                                  id_lag = dplyr::lag(.data$id, default = 0),
                                  set = cumsum(ifelse(xmax == dplyr::lag(.data$xmin, default = 0), 0, 1)))

  all_layout_g <- dplyr::group_by(all_layout_m, set, ymin, id)

  all_layout_sum <- dplyr::summarise(all_layout_g,
                                     xmin = min(xmin),
                                     xmax = max(xmax),
                                     ymax = min(ymax),
                                     funnel = funnel[1])

  all_layout_g2 <- dplyr::group_by(all_layout_sum, ymax)

  all_layout_ord <- dplyr::arrange(all_layout_g2, ymax)

  all_layout_out <- dplyr::mutate(all_layout_ord,
                                  level = dplyr::group_indices())


  return(dplyr::ungroup(all_layout_out))

}
