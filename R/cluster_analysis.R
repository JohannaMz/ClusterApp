#'
#' @description The main cluster analysis function for the app
#'
#' @return The function returns a list of 4 files, which are the clusters, the GPS points,
#' a track of each individual and a status message
#' @importFrom dplyr filter lag select arrange group_by left_join mutate n
#' rename slice summarize ungroup all_of
#' @importFrom lubridate date is.Date is.POSIXct ymd_hms
#' @importFrom stats aggregate cutree dist hclust
#' @importFrom hms as_hms
#' @importFrom sf st_as_sf st_buffer st_cast st_centroid st_coordinates st_crs
#' st_drop_geometry st_geometry st_join st_read st_transform st_union st_is_empty st_intersects
#' @importFrom sftrack as_sftraj
#' @importFrom tidyr separate unite
#' @importFrom readxl read_excel
#' @importFrom openxlsx write.xlsx
#' @importFrom foreign read.dbf
#' @importFrom methods is
#' @importFrom stats ts
#' @importFrom utils write.table
#' @importFrom stringr str_detect
#'
#' @noRd


# If this file should be used independently of the 'ClusterApp' package, these packages
# have to be installed, i.e. loaded #####
# library(dplyr)
# library(sf)
# library(lubridate)
# library(sftrack)
# library(tidyr)
# library(readr)
# library(openxlsx)
# library(readxl)
######

cluster_analysis <- function(
    intensive.start , #entered start date within as.Date("%Y-%m-%d)
    intensive.end , #entered start date within as.Date("%Y-%m-%d)
    datapoints, #path to the datapoints file in .csv or .shp format
    sep, #chosen separator if a csv file is entered as datapoints
    ID , #column name for 'animal ID' column
    LMT_Date , #column name for 'timestamp' column
    East , #column name for 'easting/latitude' column
    North, #column name for 'northing/longitude' column
    dateFormat, #date format of the column chosen for LMT_Date
    prepostPeriod, #number of days chosen as pre and post study period frame
    EPSGcode = 4326, #EPSG code of loaded datapoints, default at WGS84
    buffer, #distance in meters chosen around each GPS points
    count, #number of GPS points necessary for a cluster to be created
    indID, #chosen label for this study
    lastClustersFile = "No latest cluster file.",
    #path to a potentially last cluster file, if no latest cluster file exists keep default
    minute_diff = NA,
    #set time difference between GPS fixes, if all GPS locations should be used keep default
    onlyClusters = FALSE,
    #TRUE = clusters are created with only consecutive GPS locations;
    #FALSE = clusters are created irrespective of time stamp
    oldclusters = FALSE,
    #TRUE = state column for clusters from latest cluster files are set to 'Old';
    #FALSE = no changes to state column for latest cluster file
    UTM_zone #UTM zone the data lies in
    ){

  #binding the variables locally to the function for the R-CMD-Check
  ClusID <- crs <- ts <- ts_num <- ident <- diff_min <- time_group_minu <- x <-
    prev_ClusID <- new_cluster <- Status <- date_max <- date_min <- prec_time <-
    inout <- ratio <- State <- Event <- Done <- Worker <- center_x <- center_y <-
    geometry <- ClusID.y <- ID.x <- sum.x <- sum.y <- prec_time.x <- inout.x <-
    ratio.x <- date_min.x <- date_max.x <- Event.y <- Done.y <- Worker.y <- State.y <-
    sum.y <- center <- LMT_Time <- month <- day <- hour <- NULL

  message <- "Working."



  if (is.data.frame(datapoints)) {
  datapoints <- datapoints

  settings <- c("start of study period" = as.character(intensive.start),
                "end of study period" = as.character(intensive.end),
                "path to GPS data" = as.character(getwd()),
                "ID column name" = ID,
                "LMT_Date column name" = LMT_Date,
                "East column name" = East,
                "North column name" = North,
                "date format" = dateFormat,
                "prepostPeriod" = prepostPeriod,
                "EPSGcode of GPS data file" = as.character(EPSGcode),
                "buffer radius around GPS locations" = buffer,
                "count of locations necessary within a buffer" = count,
                "label" = indID,
                "path to latest clusters file" = lastClustersFile,
                "minute difference between GPS fixes" = minute_diff,
                "develop clusters with only consecutive locations" = onlyClusters,
                "old clusters marked as done" = oldclusters,
                "UTM zone for output data" = UTM_zone)



  } else if(sum(strsplit(basename(datapoints), split="\\.")[[1]][-1] == "csv") == TRUE){

  settings <- c("start of study period" = as.character(intensive.start),
                "end of study period" = as.character(intensive.end),
                "path to GPS data" = datapoints,
                "ID column name" = ID,
                "LMT_Date column name" = LMT_Date,
                "East column name" = East,
                "North column name" = North,
                "date format" = dateFormat,
                "prepostPeriod" = prepostPeriod,
                "EPSGcode of GPS data file" = as.character(EPSGcode),
                "buffer radius around GPS locations" = buffer,
                "count of locations necessary within a buffer" = count,
                "label" = indID,
                "path to latest clusters file" = lastClustersFile,
                "minute difference between GPS fixes" = minute_diff,
                "develop clusters with only consecutive locations" = onlyClusters,
                "old clusters marked as done" = oldclusters,
                "UTM zone for output data" = UTM_zone)

  datapoints <-  read_delim(datapoints, delim = sep, escape_double = FALSE,
                            trim_ws = TRUE)

  } else if (sum(strsplit(basename(datapoints), split="\\.")[[1]][-1] == "shp") == TRUE){

  settings <- c("start of study period" = as.character(intensive.start),
                  "end of study period" = as.character(intensive.end),
                  "path to GPS data" = datapoints,
                  "ID column name" = ID,
                  "LMT_Date column name" = LMT_Date,
                  "East column name" = East,
                  "North column name" = North,
                  "date format" = dateFormat,
                  "prepostPeriod" = prepostPeriod,
                  "EPSGcode of GPS data file" = as.character(EPSGcode),
                  "buffer radius around GPS locations" = buffer,
                  "count of locations necessary within a buffer" = count,
                  "label" = indID,
                  "path to latest clusters file" = lastClustersFile,
                  "minute difference between GPS fixes" = minute_diff,
                  "develop clusters with only consecutive locations" = onlyClusters,
                  "old clusters marked as done" = oldclusters,
                  "UTM zone for output data" = UTM_zone)

  datapoints <- read_sf(datapoints)

  East = NA
  North = NA

  } else {
  datapoints <-  NULL
  }



  if (is.null(datapoints)) {
    status <- "Please upload data in the right format."
    cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA,
                         status = status, settings = settings)

  } else if (is.na(dateFormat)|
      is.na(EPSGcode)|
      is.na(UTM_zone)){

    status <- "Input missing in Tab 1: Upload GPS data. Check if you have entered a
    date format, an input EPSG code and the output UTM zone."
    cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA,
                         status = status, settings = settings)

  } else if (is.na(indID)|
             is.na(buffer)|
             is.na(count)){

    status <- "Input missing in Tab 2: Adjust Cluster Analysis Parameters.
    Check if you have entered a label, buffer size and the number of GPS locations
    needed for a cluster."
    cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA,
                         status = status, settings = settings)

  } else {

      if (is(datapoints, "sf")) {

          datapoints <- datapoints %>%
            dplyr::select(all_of(ID), all_of(LMT_Date))

          colnames(datapoints) <- c("ID", "LMT_Date", "geometry")

        } else {

           datapoints <- datapoints %>%
                    dplyr::select(all_of(ID), all_of(LMT_Date),
                                  all_of(East), all_of(North))

           colnames(datapoints) <- c("ID", "LMT_Date", "East", "North")


           datapoints <- filter(datapoints, !is.na(East) &
                                  !is.na(North) & East != 0 & North != 0)

        }

      if (is.character(datapoints$ID) == FALSE & is.numeric(datapoints$ID) == FALSE) {
                status <- "ID is not a character or numeric value.
                            Choose a different column."
                cluster_list <- list(Clusters_sf = NA, Join_sf = NA,
                                     data_sf_traj = NA,
                                     status = status, settings = settings)

      } else if (is.character(datapoints$LMT_Date) == FALSE &
                       is.Date(datapoints$LMT_Date) == FALSE &
                       is.POSIXct(datapoints$LMT_Date) == FALSE){

                status <- "LMT_Date is not a character, Date or POSIXct value.
                            Adjust this please."
                cluster_list <- list(Clusters_sf = NA, Join_sf = NA,
                                     data_sf_traj = NA,
                                     status = status, settings = settings)

      } else if ((is.numeric(datapoints$East) == FALSE|
                is.numeric(datapoints$North) == FALSE) &
                  !is.na(East)){

                status <- "East and/or North coordinates are not numeric.
                            Find the right columns."
                cluster_list <- list(Clusters_sf = NA, Join_sf = NA,
                                     data_sf_traj = NA,
                                     status = status, settings = settings)

      } else if (sum(is.na(as.POSIXct(datapoints$LMT_Date,
                                    format = dateFormat, tz = "UTC"))) > 1) {

                status <- "Some or all dates failed to parse: The given date format does not
                match the format of your data or you have to delete/fix the problematic rows."
                cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA,
                                     status = status, settings = settings)

      } else {
          datapoints <- datapoints %>%
          dplyr::mutate("ts" = as.POSIXct(LMT_Date,
                                          format = dateFormat,
                                          tz = "UTC"),
                        "LMT_Date" = date(ts),
                        "LMT_Time" = as.factor(hms::as_hms(ts)),
                        "ID" = as.factor(ID))



          datapoints$Status <- NA


          #status for the points: NA = not within the period for the cluster analysis,
          #thus deleted. O = input$prepostPeriod days before or after the intensive period.
          #1 = intensive period
          datapoints$Status[datapoints$LMT_Date >= intensive.start - prepostPeriod &
                            datapoints$LMT_Date < intensive.start] <- "0"
          datapoints$Status[datapoints$LMT_Date > intensive.end &
                            datapoints$LMT_Date <= intensive.end +
                            prepostPeriod] <- "0"
          datapoints$Status[datapoints$LMT_Date >= intensive.start &
                            datapoints$LMT_Date <= intensive.end] <- "1"
          datapoints$Status <- as.numeric(datapoints$Status)
          datapoints <- datapoints[!(is.na(datapoints$Status)), ]

          if (nrow(datapoints) == 0) {

            status <- "No data within this study period. Try another time frame."
            cluster_list <- list(Clusters_sf = NA, Join_sf = NA,
                                 data_sf_traj = NA,
                                 status = status, settings = settings)

          } else {

            if (!is.na(minute_diff)) {

              datapoints <- datapoints %>%
                  arrange(ID, ts) %>%
                  group_by(ID) %>%
                  mutate(
                    diff_min = round(as.numeric(difftime(ts, min(ts),
                                             units = "min")), 0),
                    time_group_minu = cutree(hclust(dist(diff_min)),
                    h = minute_diff-1)) %>%
                  group_by(ID, time_group_minu) %>%
                  slice(1) %>%
                  ungroup() %>%
                  dplyr::select(-c(diff_min, time_group_minu))

            } else {
              minute_diff_data <- datapoints %>%
                  arrange(ID, ts) %>%
                  group_by(ID) %>%
                  mutate(diff_min = as.numeric(difftime(LMT_Date,
                                          lag(LMT_Date),
                                          units = "min")))

              minute_diff = round(mean(minute_diff_data$diff_min,
                                 na.rm = TRUE), 0)

            }


            #make the data spatial
            data_sf <- sf::st_as_sf(datapoints,
                                  coords = c("North", "East"),
                                  crs = st_crs(EPSGcode))
            UTM_coord <- as.numeric(paste0("258", UTM_zone))

            data_sf <- sf::st_transform(data_sf,
                                crs = st_crs(UTM_coord))
            #change WGS84 to UTM to have meters

              if (sum(st_is_empty(data_sf)) > 0) {

                  status <- "Something is wrong with your coordinates.
                  Transformation leaves the geometry column empty."
                  cluster_list <- list(Clusters_sf = NA, Join_sf = NA,
                                       data_sf_traj = NA,
                                       status = status, settings = settings)


              } else {
                  #make a track from the data
                  data_sf_traj <- sf::st_as_sf(as_sftraj(data_sf,
                                             group = c(id = "ID"),
                                             time = "ts"))
                  data_sf_traj <- sf::st_as_sf(data_sf_traj[, c(1:5, 7),
                                             drop = TRUE])

                   Clusters_sf_combined <- data.frame()
                   Join_sf_combined <- data.frame()


                   for (i in unique(datapoints$ID)) {

                      data_sf_ID <- filter(data_sf, ID == i)
                      #apply buffer around each point, disaggregate and
                      #make the data spatial again
                      Buffer_sf <- st_buffer(data_sf_ID, buffer)
                      Multi <- st_union(Buffer_sf)
                      Multi <- st_cast(Multi, "POLYGON")
                      Multi_sf <- Multi %>%
                          sf::st_as_sf() %>%
                          rename(geometry = x)

                      #preliminary cluster ID to cluster points together
                      Multi_sf$ClusID <- seq(1:nrow(Multi_sf))
                      #seq(stats::rnorm(nrow(Multi_sf)))


                      #determine in which cluster the points fall
                      Join_sf <- st_join(data_sf_ID, Multi_sf)


                      if(onlyClusters == TRUE){

                          Join_sf <- Join_sf %>%
                          arrange(ts) %>%
                          mutate(prev_ClusID = lag(ClusID),
                                 new_cluster = ifelse(ClusID != prev_ClusID,
                                                TRUE, FALSE),
                                 new_cluster = ifelse(is.na(prev_ClusID),
                                                TRUE, new_cluster),
                                 ClusID = cumsum(new_cluster)) %>%
                          select(-prev_ClusID, -new_cluster)

                      }

                      #determine the clusters to be visited
                      Clusters_sf <- Join_sf %>%
                          group_by(ClusID) %>%
                          #delete clusters with only one point within
                          filter(n()>=count) %>%
                          #delete clusters with only points
                          #outside of the intensive period:
                          #sum = 0 and identify the first event in the cluster
                          summarize(sum = sum(Status),
                                    date_min = min(ts),
                                    date_max = max(ts)) %>%
                          filter(sum >= 1) %>%
                          mutate(ID = i,
                                 prec_time = round(sum/(round(as.numeric(
                                 difftime(date_max, date_min,
                                       units = "mins"))/minute_diff, 0)+1),
                                       2)) %>%
                          #arrange the data according to date
                          arrange(date_min)


                          # combine the points to own polygons
                      Clusters_sf <- Clusters_sf %>%
                          st_buffer(buffer) %>%
                          st_union(by_feature = TRUE) %>%
                          st_cast("POLYGON")



                      #identify points inside to outside cluster
                      for (j in 1:nrow(Clusters_sf)) {

                          Join_sf_filter <- filter(Join_sf,
                                        ts >= Clusters_sf$date_min[j] &
                                        ts <= Clusters_sf$date_max[j])

                          inside <- nrow(filter(Join_sf_filter,
                                        ClusID == Clusters_sf$ClusID[j]))
                          outside <- nrow(filter(Join_sf_filter,
                                        ClusID != Clusters_sf$ClusID[j]))

                          Clusters_sf$inout[j] <- paste0(inside, "/", outside)

                          Clusters_sf$ratio[j] <-
                            if(outside == 0){
                                    "All GPS locations within cluster."
                            } else if(inside < outside){
                                    "More GPS locations outside of cluster."
                            } else if(inside > outside){
                                    "More GPS locations inside of cluster."
                            } else if(inside == outside){
                                    "Even number of GPS locations inside and
                                    outside of cluster."
                            }

                        }


                        #so that now the new cluster ID can be
                        #assigned with 1 being the oldest
                      if(nrow(Clusters_sf) != 0){
                              Clusters_sf$ClusID <- paste(i,
                                                  seq(1:nrow(Clusters_sf)),
                                                  sep = "_")
                      }


                      Clusters_sf <- st_as_sf(Clusters_sf)
                      Clusters_sf$State <-  factor(NA,
                                              levels = c("New",
                                                         "Done",
                                                         "Points added",
                                                         "Not done"))
                      Clusters_sf$State <- "New"
                      Clusters_sf$Event <- as.character(NA)
                      Clusters_sf$Done <- as.character(NA)
                      Clusters_sf$Worker <- as.character(NA)
                      Clusters_sf$Notes <- as.character(NA)

                      if (lastClustersFile != "No latest cluster file." &
                                      onlyClusters == FALSE) {

                            #identify already exsiting clusters from the
                            #analysis before:
                            #upload data

                            Clusters_sf_before <-
                                if(sum(strsplit(basename(lastClustersFile),
                                                    split="\\.")[[1]][-1]
                                       == "xlsx") == TRUE){

                                #read_excel(lastClustersFile)
                                  st_as_sf(read_excel(lastClustersFile),
                                         wkt = "geometry",
                                         crs = UTM_coord)

                                } else if (sum(strsplit(basename(lastClustersFile),
                                                    split="\\.")[[1]][-1]
                                       == "shp") == TRUE){

                                  read_sf(lastClustersFile)

                                } else {
                                    NULL

                                }


                            if((sum(c("ID", "ClusID", "sum", "prec_time",
                                              "inout", "ratio", "date_min",
                                              "date_max", "State" , "Event",
                                              "Done", "Worker", "Notes",
                                              "center_x", "center_y",
                                              "geometry") %in%
                                            names(Clusters_sf_before)) ==  16) &
                                       (st_crs(Clusters_sf) ==
                                        st_crs(Clusters_sf_before))){


                                Clusters_sf_before <-
                                      dplyr::select(Clusters_sf_before, ID, ClusID,
                                                     sum, prec_time, inout , ratio,
                                                     date_min,  date_max, State,
                                                     Event, Done, Worker, Notes,
                                                     center_x, center_y, geometry)

                                Clusters_sf_before <- filter(Clusters_sf_before,
                                                             ID == i)

                                if(nrow(Clusters_sf_before) != 0){

                                    Clusters_sf <- st_join(Clusters_sf,
                                                           Clusters_sf_before)

                                    Clusters_sf <- Clusters_sf %>%
                                        dplyr::select(ClusID.y, ID.x, geometry,
                                                      sum.x, sum.y, prec_time.x,
                                                      inout.x, ratio.x, date_min.x,
                                                      date_max.x, Event.y,
                                                      Done.y, Worker.y,
                                                      Notes.y, State.y) %>%
                                        rename("ClusID" = "ClusID.y",
                                               "ID" = "ID.x",
                                               "prec_time" = "prec_time.x",
                                               "inout" = "inout.x",
                                               "ratio" = "ratio.x",
                                               "date_min" = "date_min.x",
                                               "date_max" = "date_max.x",
                                               "Event" = "Event.y",
                                               "Done" = "Done.y",
                                               "Worker" = "Worker.y",
                                               "State" = "State.y",
                                               "Notes" = "Notes.y")


                                    #if clusters have points added in their state,
                                    #this statement should be deleted again
                                    #for the next analysis
                                        #Clusters_sf$State[Clusters_sf$State ==
                                        #"Points added"] <- "New"


                                    Clusters_sf <-
                                        Clusters_sf[order(Clusters_sf$date_min), ]

                                    #extract the maximum number that has been given as a ClusterID
                                    ClusID_numbers <- c()
                                    for (y in 1:length(Clusters_sf_before$ClusID)) {
                                        split_vector <-
                                            strsplit(Clusters_sf_before$ClusID[y], "_")[[1]]
                                        number <-
                                            as.numeric(split_vector[length(split_vector)])
                                        ClusID_numbers[y] <- number
                                    }

                                    #fill up the new clusters from the maximum
                                    #Cluster ID until the end
                                    Clusters_sf$ClusID[is.na(Clusters_sf$ClusID)] <-
                                          paste(i, (max(ClusID_numbers)+1):
                                                  (max(ClusID_numbers)+
                                                   sum(is.na(Clusters_sf$ClusID))),
                                            sep = "_")

                                    #because the st_join now only takes the
                                    #CLusID of the largest
                                    #overlapping polygon, some Clusters
                                    #might have "disaapepeared"
                                    #as they grew together
                                    #to keep track of those we add them
                                    #into the notes column
                                    Clusters_within <- st_intersects(Clusters_sf,
                                                                     Clusters_sf_before)



                                    for (x in 1:length(Clusters_within)) {
                                        if (length(Clusters_within[[x]]) >1) {
                                            Cluster_index <-
                                          as.character(Clusters_sf_before$ClusID[Clusters_within[[x]]])

                                          if(stringr::str_detect(Clusters_sf$Notes[x],
                                                    paste("Note! These clusters from the
                                                    latest cluster analysis grew together: ",
                                                     paste(Cluster_index,
                                                           collapse = ", "),
                                                     ".")) == FALSE){
                                               Clusters_sf$Notes[x] <-
                                              paste("Note! These clusters from the
                                                    latest cluster analysis grew together: ",
                                                    paste(Cluster_index,
                                                          collapse = ", "),
                                                    ".")
                                              }
                                          }
                                      }

                                      #"old new " clusters are marked done, new
                                      #clusters are marked new.
                                      #manually adjusted clusters stay the same
                                      if (oldclusters == TRUE) {
                                          Clusters_sf$State[Clusters_sf$State == "New"] <-
                                            "Done"
                                      }



                                      Clusters_sf$State[is.na(Clusters_sf$State)] <-
                                          "New"

                                        #clusters that have grown since the last analysis.
                                        #using geometry and not ClusID for the case,
                                        #that the cluster ID changed
                                        #after growing together with another
                                        #cluster. Still havent checkd that
                                        #case with real data, but might be possible.

                                        Clusters_sf$points[Clusters_sf$sum.x !=
                                                            Clusters_sf$sum.y] <-
                                          "Points added!"



                                        for (j in 1:nrow(Clusters_sf)) {

                                          if(stringr::str_detect(Clusters_sf$Notes[j],
                                                                   "Points added!") != TRUE|
                                            is.na(Clusters_sf$Notes[j])){
                                            Clusters_sf[j,] <- tidyr::unite(Clusters_sf[j,],
                                                                              "Notes",
                                                                              Notes, points,
                                                                          sep = " ",
                                                                          na.rm = TRUE,
                                                                          remove = FALSE)
                                            }
                                        }


                                        Clusters_sf <- dplyr::select(Clusters_sf, -c(points))

                                        Clusters_sf <- Clusters_sf %>%
                                          dplyr::select(-c(sum.y)) %>%
                                          rename("sum" = "sum.x")
                                        } else {
                                          message <- "Column names wrong."
                                        }

                              } else {
                                              message <- "Column names wrong."
                                         }

                     }


                      if (lastClustersFile != "No latest cluster file." &
                                      onlyClusters == TRUE) {

                        #identify already exsiting clusters from the analysis before:
                        #upload data

                        Clusters_sf_before <-
                              if(sum(strsplit(basename(lastClustersFile),
                                split="\\.")[[1]][-1] == "xlsx") == TRUE){

                              #read_excel(lastClustersFile)
                                st_as_sf(read_excel(lastClustersFile),
                                       wkt = "geometry",
                                       crs = UTM_coord)

                              } else if (sum(strsplit(
                                      basename(lastClustersFile),
                                      split="\\.")[[1]][-1] == "shp") == TRUE){

                                read_sf(lastClustersFile)

                              } else {
                                NULL
                              }


                              if((sum(c("ID", "ClusID", "sum", "prec_time",
                                        "inout", "ratio", "date_min",
                                        "date_max",  "State" , "Event",
                                        "Done", "Worker", "Notes" ,
                                        "center_x", "center_y",
                                        "geometry") %in%
                                    names(Clusters_sf_before)) ==  16) &
                                   (st_crs(Clusters_sf) ==
                                    st_crs(Clusters_sf_before))){


                                Clusters_sf_before <-
                                  dplyr::select(Clusters_sf_before,
                                                ClusID, sum, date_min,
                                                date_max, ID, prec_time,
                                                geometry, inout, ratio,
                                                State, Event, Done,
                                                Worker, Notes)
                                Clusters_sf_before <- filter(Clusters_sf_before,
                                                             ID == i)

                                if(nrow(Clusters_sf_before) != 0){

                                  if (oldclusters == TRUE) {
                                        Clusters_sf_before$State[Clusters_sf_before$State == "New"] <-
                                          "Done"
                                  }

                                  Clusters_sf <-
                                    Clusters_sf[(nrow(Clusters_sf_before)+1):
                                                (nrow(Clusters_sf)),]

                                    Clusters_sf <- rbind(Clusters_sf_before,
                                                     Clusters_sf)
                                } else {
                                  message <- "Column names wrong."
                                }


                              } else {
                                      message <- "Column names wrong."
                              }

                    }

                    if(message == "Column names wrong."){

                        status = "The latest cluster file could not be
                                  loaded because it either lacks the same
                                  coordinate system, correct column names,
                                  or does not contain data for the animal
                                  IDs you selected. Have you made any changes
                                  to the column names? Did you choose the right
                                  column for the animal IDs?
                                  Can you confirm if the previous file is
                                  indeed from the same dataset?
                                  Check again."
                        cluster_list <- list(Clusters_sf = NA,
                                             Join_sf = NA,
                                             data_sf_traj = NA,
                                             status = status,
                                             settings = settings)

                    } else {

                        Clusters_sf$center <- Clusters_sf %>%
                                              st_centroid() %>%
                                              st_geometry()

                        #keep the same column order for the cluster_sf
                        Clusters_sf <- Clusters_sf %>%
                                        mutate(#date_min = as.character(date_min),
                                               #date_max = as.character(date_max),
                                               center_x =
                                                 round(st_coordinates(center)[,1], 2),
                                               center_y =
                                                 round(st_coordinates(center)[,2], 2)) %>%
                                        dplyr::select(ID, ClusID, sum, prec_time,
                                                      inout, ratio, date_min,
                                                      date_max, State, Event,
                                                      Done, Worker,
                                                      Notes, center_x, center_y)

                          Clusters_sf_combined <-
                                      rbind(Clusters_sf_combined, Clusters_sf)



                          # identify the new ClusIDs and write point file for GPS
                          Join_sf <- st_join(data_sf_ID,
                                     dplyr::select(Clusters_sf,
                                                   ClusID, State))



                          #all points that do not fall within a cluster are Single Points
                          Join_sf$ClusID[is.na(Join_sf$ClusID)] <-
                                paste(i, "SP", sep = "_")

                          #write a unique point name that will make
                          #it easier in the field
                          #to understand the pattern
                          Join_sf <- Join_sf %>%
                                      separate(LMT_Date, sep = "-",
                                             into = c("year", "month",
                                                      "day")) %>%
                                      separate(LMT_Time, sep = ":",
                                             into = c("hour", "minute",
                                                      "second")) %>%
                                      unite(ident, ClusID, month, day, hour,
                                        remove = FALSE,  sep = "_") %>%
                                      mutate(x = round(st_coordinates(Join_sf)[,1], 2),
                                             y = round(st_coordinates(Join_sf)[,2], 2)) %>%
                                      unite(ClusID, ClusID:State, na.rm = TRUE, sep = " - ")

                          Join_sf_combined <- rbind(Join_sf_combined,
                                                    Join_sf)



                          status <- "Done!"
                          cluster_list <- list(Clusters_sf = Clusters_sf_combined,
                                               Join_sf = Join_sf_combined,
                                               data_sf_traj = data_sf_traj,
                                               status = status,
                                               settings = settings)

              }
            }
          }
        }
      }
    }

  cluster_list

}

