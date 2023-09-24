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
#' @importFrom readxl read_excel
#' @importFrom openxlsx write.xlsx
#'
#' @noRd


cluster_analysis <- function(intensive.start ,
                             intensive.end ,
                             datapoints,
                             sep,
                             ID ,
                             LMT_Date ,
                             East ,
                             North,
                             dateFormat,
                             prepostPeriod = 0 ,
                             EPSGcode,
                             buffer,
                             count,
                             indID, #label
                             lastClustersFile,
                             minute_diff,
                             onlyClusters,
                             oldclusters,
                             UTM_zone){
  message <- "Working."

  settings <- c("start of study period" = as.character(intensive.start),
                          "end of study period" = as.character(intensive.end),
                          "path to GPS data" = datapoints,
                          "ID column name" = ID,
                          "LMT_Date column name" = LMT_Date,
                          "East column name" = East,
                          "North column name" = North,
                          "date format" = dateFormat,
                          #"prepostPeriod" = prepostPeriod,
                          "EPSGcode of GPS data file" = as.character(EPSGcode),
                          "buffer radius around GPS locations" = buffer,
                          "count of locations necessary within a buffer" = count,
                          "label" = indID,
                          "path to latest clusters file" = lastClustersFile,
                          "minute difference between GPS fixes" = minute_diff,
                          "filter for clusters with only consecutive locations" = onlyClusters,
                          "old clusters marked as done" = oldclusters,
                          "UTM zone for output data" = UTM_zone)


  datapoints <- if(sum(strsplit(basename(datapoints), split="\\.")[[1]][-1] == "csv") == TRUE){
    read_delim(datapoints, delim = sep, escape_double = FALSE, trim_ws = TRUE)

  }else if(sum(strsplit(basename(datapoints), split="\\.")[[1]][-1] == "dbf") == TRUE){
    read.dbf(datapoints, as.is = FALSE)

  } else if (sum(strsplit(basename(datapoints), split="\\.")[[1]][-1] == "shp") == TRUE){
    read_sf(datapoints)
  } else {
    NULL
  }


  if (is.null(datapoints)) {
    status <- "Please upload data in the right format."
    cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

  } else if (is.na(dateFormat)|
      is.na(EPSGcode)|
      is.na(UTM_zone)){

    status <- "Input missing in Tab 1: Upload GPS data. Check if you have entered a date format, an input EPSG code and the output UTM zone."
    cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

  } else if (is.na(indID)|
             is.na(buffer)|
             is.na(count)){

    status <- "Input missing in Tab 2: Adjust Cluster Analysis Parameters. Check if you have entered a label, buffer size and the number of GPS locations needed for a cluster."
    cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

  }  else {


    datapoints <- datapoints %>%
      dplyr::select(all_of(ID), all_of(LMT_Date), all_of(East), all_of(North))

    colnames(datapoints) <- c("ID", "LMT_Date", "East", "North")


    if (is.character(datapoints$ID) == FALSE) {
      status <- "ID is not a character value. Choose a different column."
      cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

    }
    else if (is.character(datapoints$LMT_Date) == FALSE &
             is.Date(datapoints$LMT_Date) == FALSE &
             is.POSIXct(datapoints$LMT_Date) == FALSE){

      status <- "LMT_Date is not a character, Date or POSIXct value. Adjust this please."
      cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

    }
    else if (is.numeric(datapoints$East) == FALSE| is.numeric(datapoints$North) == FALSE){
      status <- "East and/or North coordinates are not numeric. Find the right columns."
      cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

    }
    else {

      datapoints <- datapoints %>%
        dplyr::mutate("ts" = as.POSIXct(LMT_Date, format = dateFormat),
               "LMT_Date" = date(ts),
               "LMT_Time" = as.factor(hms::as_hms(ymd_hms(ts))))


      datapoints <- datapoints[!is.na(datapoints$East),]


      if (!is.na(minute_diff)) {

        datapoints <- datapoints %>%
          arrange(ID, ts) %>%
          group_by(ID) %>%
          mutate(
            diff_min = round(as.numeric(difftime(ts, min(ts), units = "min")), 0),
            time_group_minu = cutree(hclust(dist(diff_min)), h = minute_diff-1)) %>%
          group_by(ID, time_group_minu) %>%
          slice(1) %>%
          ungroup() %>%
          dplyr::select(-c(diff_min, time_group_minu))

      } else {
        minute_diff_data <- datapoints %>%
          arrange(ID, ts) %>%
          group_by(ID) %>%
          mutate(diff_min = as.numeric(difftime(LMT_Date, lag(LMT_Date), units = "min")))

        minute_diff = round(mean(minute_diff_data$diff_min, na.rm = TRUE), 0)

      }



      datapoints$Status <- NA

      #status for the points: NA = not within the period for the cluster analysis, thus deleted. O = input$prepostPeriod days before or after the intensive period. 1 = intensive period
      datapoints$Status[datapoints$LMT_Date >= intensive.start - prepostPeriod & datapoints$LMT_Date < intensive.start] <- "0"
      datapoints$Status[datapoints$LMT_Date > intensive.end & datapoints$LMT_Date <= intensive.end + prepostPeriod] <- "0"
      datapoints$Status[datapoints$LMT_Date >= intensive.start & datapoints$LMT_Date <= intensive.end] <- "1"
      datapoints$Status <- as.numeric(datapoints$Status)
      datapoints <- datapoints[!(is.na(datapoints$Status)), ]

      if (nrow(datapoints) == 0) {
        status <- "No data within this study period. Try another time frame."
        cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

      } else {

        #make the data spatial
        data_sf <- sf::st_as_sf(datapoints, coords = c("North", "East"), crs = EPSGcode)
        UTM_coord <- as.numeric(paste0("258", UTM_zone))

        data_sf <- sf::st_transform(data_sf, crs = st_crs(UTM_coord)) #change WGS84 to UTM to have meters

        #make a track from the data
        data_sf_traj <- sf::st_as_sf(as_sftraj(data_sf, group = c(id = "ID"), time = "ts"))
        data_sf_traj <- sf::st_as_sf(data_sf_traj[, c(1:5, 7), drop = TRUE])

        Clusters_sf_combined <- data.frame()
        Join_sf_combined <- data.frame()


        for (i in unique(datapoints$ID)) {

          data_sf_ID <- filter(data_sf, ID == i)

          #apply buffer around each point, disaggregate and make the data spatial again
          #(maybe there is an easier way that the data is not converted to a list??)
          Buffer_sf <- st_buffer(data_sf_ID, buffer)
          Multi <- st_union(Buffer_sf)
          Multi <- st_cast(Multi, "POLYGON")
          Multi_sf <- Multi %>%
            sf::st_as_sf() %>%
            rename(geometry = x)

          #preliminary cluster ID to cluster points together: get this to work when there are no clusters! ifelse....
          Multi_sf$ClusID <- seq(1:nrow(Multi_sf)) #seq(stats::rnorm(nrow(Multi_sf)))


          #determine in which cluster the points fall
          Join_sf <- st_join(data_sf_ID, Multi_sf)


          #determine the clusters to be visited
          Clusters_sf <- Join_sf %>%
            group_by(ClusID) %>%
            #delete clusters with only one point within
            filter(n()>=count) %>%
            #delete clusters with only points outside of the intensive period: sum = 0 and identify the first event in the cluster
            summarize(sum = sum(Status), date_min = min(ts), date_max = max(ts)) %>%
            filter(sum >= 1) %>%
            mutate(ID = i,
                   prec_time = round(sum/(round(as.numeric(difftime(date_max, date_min, units = "mins"))/minute_diff, 0)+1), 2)) %>%
            #drop the geometry column because we will add the polygon geometry now
            st_drop_geometry() %>%
            left_join(Multi_sf, by = c("ClusID")) %>%
            #arrange the data according to date
            arrange(date_min)

          #identify points inside to outside cluster
          for (j in 1:nrow(Clusters_sf)) {

            Join_sf_filter <- filter(Join_sf, ts >= Clusters_sf$date_min[j] & ts <= Clusters_sf$date_max[j])

            inside <- nrow(filter(Join_sf_filter, ClusID == Clusters_sf$ClusID[j]))
            outside <- nrow(filter(Join_sf_filter, ClusID != Clusters_sf$ClusID[j]))
            Clusters_sf$inout[j] <- paste0(inside, "/", outside)
            Clusters_sf$ratio[j] <- if(outside == 0){
              "All GPS locations within cluster."
            } else if(inside < outside){
              "More GPS locations outside of cluster."
            } else if(inside > outside){
              "More GPS locations inside of cluster."
            } else if(inside == outside){
              "Even number of GPS locations inside and outside of cluster."
            }

          }

          if(onlyClusters == TRUE){
            Clusters_sf <- filter(Clusters_sf, ratio == "All GPS locations within cluster.")
          }

          #so that now the new cluster ID can be assigned with 1 being the oldest
          if(nrow(Clusters_sf) != 0){
            Clusters_sf$ClusID <- paste(i, seq(1:nrow(Clusters_sf)), sep = "_")
          }

          # #so that now the new cluster ID can be assigned with 1 being the oldest
          # Clusters_sf$ClusID <- paste(i, seq(1:nrow(Clusters_sf)), sep = "_") #seq(stats::rnorm(nrow(Clusters_sf)))

          #convert into sf data
          Clusters_sf <- st_as_sf(Clusters_sf)

          Clusters_sf$State <-  factor(NA, levels = c("New", "Done", "Points added", "Not done"))
          Clusters_sf$State <- "New"

          Clusters_sf$Event <- as.character(NA)
          Clusters_sf$Done <- as.character(NA)
          Clusters_sf$Worker <- as.character(NA)


          if (lastClustersFile != "No latest cluster file.") {

              #identify already exsiting clusters from the analysis before: upload data

            Clusters_sf_before <- if(sum(strsplit(basename(lastClustersFile), split="\\.")[[1]][-1] == "xlsx") == TRUE){

            #read_excel(lastClustersFile)
            st_as_sf(read_excel(lastClustersFile), wkt = "geometry", crs = UTM_coord)

            } else if (sum(strsplit(basename(lastClustersFile), split="\\.")[[1]][-1] == "shp") == TRUE){

            read_sf(lastClustersFile)

            } else {
              NULL
            }


            if(sum(c("ID", "ClusID", "sum", "prec_time","inout" , "ratio", "date_min",  "date_max",  "State" ,    "Event",     "Done"  ,
                     "Worker" ,"center_x","center_y","geometry") %in% names(Clusters_sf_before)) ==  15){


              Clusters_sf_before <- dplyr::select(Clusters_sf_before, ID, ClusID, sum, prec_time, inout , ratio, date_min,  date_max,  State ,    Event,     Done  ,
                                                  Worker ,center_x,center_y,geometry)


            Clusters_sf_before <- filter(Clusters_sf_before, ID == i)

            Clusters_sf <- st_join(Clusters_sf, Clusters_sf_before)


            #adjust ClusID again: this is the file that will be used later again, so it has to be safed to your working directory: Clusters_"date"
            Clusters_sf <- Clusters_sf %>%
              dplyr::select(ClusID.y, ID.x, geometry, sum.x, sum.y,  prec_time.x, inout.x, ratio.x, date_min.x, date_max.x, Event.y, Done.y, Worker.y, State.y) %>%
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
                     "State" = "State.y")


            Clusters_sf <- Clusters_sf[order(Clusters_sf$date_min), ]
            #Clusters_sf$ClusID <- as.numeric(Clusters_sf$ClusID)

            #fill up the new clusters with IDs
            Clusters_sf$ClusID[is.na(Clusters_sf$ClusID)] <- paste(i, (nrow(Clusters_sf_before) + 1):nrow(Clusters_sf), sep = "_")


            #combine numbers if clusters grew together
            grewTogether <- aggregate(Clusters_sf[1], Clusters_sf[-1],
                                      FUN = function(X) paste(unique(X), collapse="/"))
            grewTogether <- st_drop_geometry(grewTogether)
            Clusters_sf$ClusID <- grewTogether$ClusID
            Clusters_sf <- Clusters_sf[!duplicated(Clusters_sf$ClusID), ]


            #"old new " clusters are marked done, new clusters are makred new. manually adjusted clusters stay the same
            Clusters_sf$State[is.na(Clusters_sf$State)] <- "New"

            if (oldclusters == TRUE) {
              Clusters_sf$State[Clusters_sf$State == "New"] <- "Done"
            }



            #clusters that have grown since the last analysis. using geometry and not ClusID for the case, that the cluster ID changed after growing together with another cluster. Still havent checkd that case with real data, but might be possible.
            Clusters_sf$State[Clusters_sf$sum.x != Clusters_sf$sum.y] <- "Points added"

            Clusters_sf <- Clusters_sf %>%
              dplyr::select(-c(sum.y)) %>%
              rename("sum" = "sum.x")

            } else {
              message <- "Column names wrong."
            }

          }


          if(message == "Column names wrong."){
            status = "The latest cluster file could not be loaded or does not have the right column names. Did you change any column names? Check again."
            cluster_list <- list(Clusters_sf = NA, Join_sf = NA, data_sf_traj = NA, status = status, settings = settings)

          } else {

          Clusters_sf$center <- Clusters_sf %>%
            st_centroid() %>%
            st_geometry()

          #keep the same column order for the cluster_sf
          Clusters_sf <- Clusters_sf %>%
            mutate(center_x = round(st_coordinates(center)[,1], 2),
                   center_y = round(st_coordinates(center)[,2], 2)) %>%
            dplyr::select(ID, ClusID, sum, prec_time, inout, ratio, date_min, date_max, State, Event, Done, Worker, center_x, center_y)

          Clusters_sf_combined <- rbind(Clusters_sf_combined, Clusters_sf)



          # identify the new ClusIDs and write point file for GPS
          Join_sf <- st_join(data_sf_ID,
                             dplyr::select(Clusters_sf, ClusID))


          #all points that do not fall within a cluster are Single Points
          Join_sf$ClusID[is.na(Join_sf$ClusID)] <- paste(i, "SP", sep = "_")


          #write a unique point name that will make it easier in the field to understand the pattern
          Join_sf <- Join_sf %>%
            separate(LMT_Date, sep = "-", into = c("year", "month", "day")) %>%
            separate(LMT_Time, sep = ":", into = c("hour", "minute", "second")) %>%
            unite(ident, ClusID, month, day, hour, remove = FALSE,  sep = "-") %>%
            mutate(x = round(st_coordinates(.)[,1], 2),
                   y = round(st_coordinates(.)[,2], 2))

          Join_sf_combined <- rbind(Join_sf_combined, Join_sf)



        status <- "Done!"
        cluster_list <- list(Clusters_sf = Clusters_sf_combined, Join_sf = Join_sf_combined, data_sf_traj = data_sf_traj, status = status, settings = settings)

          }
        }
      }
    }

  }

  cluster_list

}
