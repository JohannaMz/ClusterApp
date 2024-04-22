#' GPS location data of three bears
#'
#' Historic data for 3 bears(Läfmamäck = adult male; Ruta = adult female; Järpa = subadult female) from 2014 . The study period went from 01-05-2014 until 31-05-2014.
#' It additionally includes data for one bear (Håkan = adult male) between 30-05-2023 until 13-06-2023 that had a proximity event included.
#'
#' @format A data frame with 8 rows and 2204 column
#' \describe{
#'   \item{Object_ID}{The ID of the monitored individuals.}
#'   \item{Collar_ID}{The ID of the collar fit on the individuals.}
#'   \item{PubName}{The ID of the individual within the pack.}
#'   \item{GMT_date}{Date and time in GMT.}
#'   \item{LMT_date}{Date and time in LMT.}
#'   \item{Longitude}{Northing position in WGS84.}
#'   \item{Latitude}{Easting position in WGS84.}
#'   \item{Height}{Height of position.}
#'
#' }
#' @source Scandinavian Brown Bear Project - Contact: Aimee Tallian
#'
"bears"
