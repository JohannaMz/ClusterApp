#' helpers
#'
#' @description The main cluster analysis function for the app
#'
#' @return The function returns a list of 4 files, which are the clusters, the GPS points, a track of each individual and a status message
#' @importFrom dplyr filter lag select arrange group_by left_join mutate n rename slice summarize ungroup
#' @importFrom lubridate date is.Date is.POSIXct ymd_hms
#' @importFrom stats aggregate cutree dist hclust
#' @importFrom hms as_hms
#' @importFrom sf st_as_sf st_buffer st_cast st_centroid st_coordinates st_crs st_drop_geometry st_geometry st_join st_read st_transform st_union
#' @importFrom sftrack as_sftraj
#' @importFrom tidyr separate unite
#'
#' @noRd

