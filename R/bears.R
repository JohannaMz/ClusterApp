#' GPS location data of bears
#'
#' Directly exported GPS data for three bears (2 males Hakan and Gommor and 1 solitary female Blistra) in Sweden. Gommors data includes proximity events, GPS locations sent more frequently, which are triggered when the collar is in proximity to another collared individual.
#'
#' @format A data frame with 8 rows and 2223 column(s)
#' \describe{
#'   \item{Object_ID}{The ID of the monitored individuals.}
#'   \item{Collar_ID}{The ID of the collar fit on the individuals.}
#'   \item{PubName}{The given names of the individuals.}
#'   \item{GMT_date}{Date and time in GMT.}
#'   \item{LMT_date}{Date and time in LMT.}
#'   \item{Longitude}{Northing position in WGS84.}
#'   \item{Latitude}{Easting position in WGS84.}
#'   \item{Height}{Height of position.}
#'
#' }
#' @source The Skandinavian Brown Bear Project - Contact: Aimee Tallian
#'
"bears"
