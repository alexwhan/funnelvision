#' @importFrom rlang .data
rlang::.data


#' Prepare data for pedigree burst
#'
#' @param funnels A data.frame
#'
#' @return A data.frame
#'
data_prep <- function(funnels) {
  if(!inherits(funnels, "data.frame")) stop(paste(funnels, "is not a data.frame"))

  if(!all(colnames(funnels) %in% c("fff", "mff", "fmf", "mmf", "ffm", "mfm", "fmm", "mmm"))) {
    stop('The funnels dataframe must have columns names in the form: "fff", "mff", "fmf" "mmf", "ffm", "mfm", "fmm", "mmm"')
  }

  if(!all(unlist(lapply(funnels, is.numeric)))) stop("There are non-numeric columns in the input data.frame")

  #We need to have the rows sorted from the most maternal contribution to least, because that is how the geometry gets determined
  funnels_sort <- dplyr::arrange(funnels, fff, mff, fmf, mmf, ffm, mfm, fmm, mmm)

  #This calculates a min and max position for each RIL. So this will ensure every RIL has equal spacing in the outer ring,
  # and will allow the inner rings to be (progressively) determined
  funnels_n <- dplyr::mutate(funnels_sort, xmin = 0:(dplyr::n() - 1),
                             xmax = 1:dplyr::n(),
                             sort_order = 1:dplyr::n(),
                             funnel = paste0(fff, mff, fmf, mmf, ffm, mfm, fmm, mmm))

  # This chunk calculates the outer ring. These are the founders that are on the paternal side for the final cross (hence "m$")
  funnels_outer_raw <- dplyr::select(funnels_n, dplyr::matches("m$"), xmin, xmax, funnel)

  funnels_outer_long <- tidyr::gather(funnels_outer_raw, level, id, -xmin, -xmax, -funnel)

  # getting the y (distance from centre)
  funnels_outer_y <- dplyr::mutate(funnels_outer_long,
                                   level = factor(level, levels = c("ffm", "mfm", "fmm", "mmm")),
                                   level_num = as.numeric(level),
                                   ymin = level_num / 4 + 3.05,
                                   ymax = dplyr::case_when(
                                     level_num < 4 ~ level_num / 4 + 3.35,
                                     level_num == 4 ~ level_num / 4 + 3.3))

  funnels_outer <- dplyr::select(funnels_outer_y, id, xmin, xmax, ymin, ymax, funnel, level_num)

  # calculate the middle ring

  # because we collapse rils into segments for this ring, we need to know the min and the max for each funnel at the middle stage
  funnels_middle_raw <- dplyr::select(funnels_n, fff, mff, fmf, mmf, xmin, xmax, funnel)

  funnels_middle_group <- dplyr::group_by(funnels_middle_raw, fff, mff, fmf, mmf)

  funnels_middle_sum <- dplyr::summarise(funnels_middle_group,
                                         xmin = min(xmin),
                                         xmax = max(xmax),
                                         funnel = funnel[1])
  funnels_middle_clean <- dplyr::select(dplyr::ungroup(funnels_middle_sum), fmf, mmf, xmin, xmax, funnel)

  funnels_middle_long <- tidyr::gather(funnels_middle_clean, level, id, -xmin, -xmax, -funnel)

  funnels_middle_m <- dplyr::mutate(funnels_middle_long,
                                  level = factor(level, levels = c("fmf", "mmf")),
                                  level_num = as.numeric(level),
                                  ymin = level_num / 2 + 1.7,
                                  ymax = dplyr::case_when(
                                    level_num == 2 ~ level_num / 2 + 2.2,
                                    level_num == 1 ~ level_num / 2 +2.25))

  funnels_middle <- dplyr::select(funnels_middle_m, id, xmin, xmax, ymin, ymax, funnel)

  #calculate the central paternal ring

  funnels_cp_raw <- dplyr::select(funnels_n, fff, mff, xmin, xmax, funnel)

  funnels_cp_g <- dplyr::group_by(funnels_cp_raw, fff, mff)

  funnels_cp_sum <- dplyr::ungroup(dplyr::summarise(funnels_cp_g,
                                                    xmin = min(xmin),
                                                    xmax = max(xmax),
                                                    funnel = funnel[1]))
  funnels_cp_select <- dplyr::select(funnels_cp_sum, id = mff, xmin, xmax, funnel)

  funnels_cp <- dplyr::mutate(funnels_cp_select,
                              ymin = 1.1,
                              ymax = 2.1,
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
                              ymax = 1,
                              id = id)

  all_layout <- dplyr::bind_rows(funnels_outer,
                                 funnels_middle,
                                 funnels_cp,
                                 funnels_cm)


  all_layout_m <- dplyr::mutate(all_layout,
                                  id_lag = dplyr::lag(.data$id, default = 0),
                                  set = cumsum(ifelse(id == id_lag, 0, 1)))

  all_layout_g <- dplyr::group_by(all_layout_m, set, ymin, id)

  all_layout_sum <- dplyr::summarise(all_layout_g,
                                     xmin = min(xmin),
                                     xmax = max(xmax),
                                     ymax = min(ymax),
                                     funnel = funnel[1])

  return(all_layout_sum)

}
