#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @importFrom shiny eventReactive isolate observe observeEvent reactive reactiveValues renderPrint renderText req renderUI
#' @import shinyFiles
#' @import shinyWidgets
#' @importFrom dplyr filter mutate group_by arrange desc select slice rename lag
#' @importFrom DT datatable renderDataTable
#' @importFrom leaflet hideGroup renderLeaflet
#' @importFrom foreign read.dbf
#' @importFrom readr parse_number read_delim
#' @importFrom sf read_sf st_as_sf st_cast st_crs st_drop_geometry st_transform st_write st_coordinates st_as_text st_simplify
#' @importFrom tmap tm_dots tm_fill tm_lines tm_markers tm_shape tm_text tmap_leaflet tmap_options tmap_save
#' @importFrom utils str write.csv
#' @importFrom readxl read_excel
#' @importFrom openxlsx write.xlsx
#' @noRd
#'
#'
app_server <- function(input, output, session) {

  #globalVariables(c("ClusID", "crs", "ident", "ts_num"))

  ## Dateipfad
  volumes = getVolumes()

  file_path <- reactive({
    if (input$demo_data == "Manual upload") {
      shinyFileChoose(input, 'GISfile', roots = volumes(), filetypes = c("csv", "shp")) #"dbf",
      as.character(parseFilePaths(volumes,input$GISfile)$datapath)
    } else if(input$demo_data == "Demo data wolf"|input$demo_data == "Demo data bears"){
      as.character(getwd())
    }

  })



  output$file_path <- renderPrint(if(length(file_path()>0)){
    file_path()}
    else {
      "Please select a file!"
    })


  file <- reactive({
    req(file_path())

    if(input$demo_data == "Manual upload" & sum(strsplit(basename(file_path()), split="\\.")[[1]][-1] == "csv") == TRUE){
      read_delim(file_path(), delim = input$separator, escape_double = FALSE, trim_ws = TRUE)

    # }else if(input$demo_data == "Manual upload" & sum(strsplit(basename(file_path()), split="\\.")[[1]][-1] == "dbf") == TRUE){
    #   read.dbf(file_path(), as.is = FALSE)

    } else if (input$demo_data == "Manual upload" & sum(strsplit(basename(file_path()), split="\\.")[[1]][-1] == "shp") == TRUE){
      read_sf(file_path())

    } else if(input$demo_data == "Demo data wolf"){
      ClusterApp::wolf
    } else if(input$demo_data == "Demo data bears"){
      ClusterApp::bears

    }
  else {
      NULL
    }
  })



observe({
  req(file())
  updatePickerInput(session = session, inputId = "ID",
                    choices = colnames(file()))
  updatePickerInput(session = session, inputId = "LMT_Date",
                           choices = colnames(file()))
  updatePickerInput(session = session, inputId = "East",
                           choices = colnames(file()))
  updatePickerInput(session = session, inputId = "North",
                           choices = colnames(file()))})



  output$file_str <- renderPrint(if(length(file_path()>0)){
    str(file())})


  output$file_summary <- renderPrint(if(length(file_path()>0)){
    summary(file())})


  dateFormat <- reactive({input$dateFormat})
  EPSGcode <- reactive({as.numeric(input$EPSG)})
  UTM_zone <- reactive({as.numeric(input$UTM_zone)})


  minute_diff_summary <- reactive({

    LMT_Date = input$LMT_Date
    ID = input$ID
    datapoints = file()

    datapoints <- datapoints %>%
      dplyr::select(c(ID, LMT_Date))

    colnames(datapoints) <- c("ID", "LMT_Date")

    data <- datapoints %>%
      arrange(ID, LMT_Date) %>%
      group_by(ID) %>%
      mutate(diff_min = as.numeric(difftime(LMT_Date, lag(LMT_Date), units = "min")))

    data$diff_min[is.na(data$diff_min)] <- mean(data$diff_min, na.rm = TRUE)

    data

  }
  )

  output$minute_diff_summary <- renderPrint(if(length(file_path()>0)){

    data <- minute_diff_summary()
    tapply(data$diff_min, data$ID, summary)

  })


  #load latest file manually
  shinyFileChoose(input, 'latestfile_manual', roots = volumes(), filetypes = c("shp", "xlsx"))

  latestfile_manual <- reactive(input$latestfile_manual)

  latestfile_manual_path <- reactive({
    as.character(parseFilePaths(volumes,latestfile_manual())$datapath)})


  #search in this folder for an older cluster file from that individual
  lastClustersFile <- reactive(
    if(length(latestfile_manual_path()>0)) {
      latestfile_manual_path()
    } else if((sum(grepl(paste0("Clusters_", input$indID),
                                            list.files(dirname(file_path()))))>0) &
                                 (sum(grepl(paste0("\\bClusters_", input$indID,"_",
                                                   stringr::str_sub(list.files(dirname(file_path()),
                                                                               pattern = paste0("Clusters_", input$indID)), -10)  %>%
                                                     readr::parse_number() %>%
                                                     max(),".shp\\b"), list.files(dirname(file_path())))>0))) {

                                   paste(dirname(file_path()),"/Clusters_", input$indID,"_",
                                         stringr::str_sub(list.files(dirname(file_path()),
                                                                     pattern = paste0("Clusters_", input$indID)), -10)  %>%
                                         readr::parse_number() %>%
                                         max(),".shp" ,sep = "")

    } else {
                                   "No latest cluster file."
}
)



output$file_path_last <- renderPrint(lastClustersFile())


settings_file <- reactive(if((lastClustersFile() != "No latest cluster file.") & #maybe not necessary to have a previous cluster file. If a setting file is there it can be used?? rework later
  (sum(grepl(paste0("Settings_", input$indID),
              list.files(dirname(file_path()))))>0) &
   (sum(grepl(paste0("\\bSettings_", input$indID,"_",
                     stringr::str_sub(list.files(dirname(file_path()), pattern = paste0("Settings_", input$indID)), -10)  %>%
                     readr::parse_number() %>%
                     max(),".txt\\b"), list.files(dirname(file_path()))))>0)){

  read_delim(paste0(dirname(file_path()),"/Settings_", input$indID,"_",
                   stringr::str_sub(list.files(dirname(file_path()),
                                               pattern = paste0("Settings_", input$indID)), -10)  %>%
                     readr::parse_number() %>%
                     max(),".txt" ,sep = ""),

             delim = "=", escape_double = FALSE, col_names = FALSE,
             trim_ws = TRUE)

})


#update according to the previous settings
observe({
   req(settings_file())
    if(!is.null(settings_file())){
      updateTextInput(session, "dateFormat", value = settings_file()[[8,2]])
      updateTextInput(session, "EPSG", value = as.numeric(settings_file()[[10,2]]))
      updateSelectInput(session, "UTM_zone", selected = settings_file()[[18,2]])

      updateNumericInput(session, "buffer",value = as.numeric(settings_file()[[11,2]]))
      updateNumericInput(session, "count", value = as.numeric(settings_file()[[12,2]]))
      updateDateRangeInput(session, "intensivePeriod", start = as.POSIXct(settings_file()[[1,2]], format = "%Y-%m-%d")+ 86400) #plus that many seconds. end is always "today , end = as.POSIXct(settings_file()[[2,2]], format = "%Y-%m-%d")+ 86400
      updateNumericInput(session ,"prepostPeriod", value = as.numeric(settings_file()[[9,2]]))

      updateNumericInput(session, "minute_diff", value = as.numeric(settings_file()[[15,2]]))
      updateCheckboxInput(session, "onlyClusters", value = ifelse(settings_file()[[16,2]] == "TRUE", TRUE, FALSE))
      updateCheckboxInput(session, "oldclusters", value = ifelse(settings_file()[[17,2]] == "TRUE", TRUE, FALSE))

      updatePickerInput(session, "ID", selected = settings_file()[[4,2]])
      updatePickerInput(session, "LMT_Date", selected = settings_file()[[5,2]])
      updatePickerInput(session, "East", selected = settings_file()[[6,2]])
      updatePickerInput(session, "North", selected = settings_file()[[7,2]])
      }
   })




  cluster_list <- eventReactive(
    input$doit, {
      if (input$demo_data == "Manual upload") {
                output <- cluster_analysis(
                    intensive.start = input$intensivePeriod[1],
                    intensive.end = input$intensivePeriod[2],
                    datapoints = file_path(),
                    sep = input$separator,
                    ID = input$ID,
                    LMT_Date = input$LMT_Date,
                    East = input$East,
                    North = input$North,
                    dateFormat = dateFormat(),
                    prepostPeriod = input$prepostPeriod,
                    EPSGcode = EPSGcode(),
                    buffer = input$buffer,
                    count = input$count,
                    indID = input$indID,
                    lastClustersFile = lastClustersFile(),
                    minute_diff = input$minute_diff,
                    onlyClusters = input$onlyClusters,
                    oldclusters = input$oldclusters,
                    UTM_zone = UTM_zone()
                  )
      } else if (input$demo_data == "Demo data wolf"|input$demo_data == "Demo data bears"){
        output <- cluster_analysis(
          intensive.start = input$intensivePeriod[1],
          intensive.end = input$intensivePeriod[2],
          datapoints = file(),
          sep = input$separator,
          ID = input$ID,
          LMT_Date = input$LMT_Date,
          East = input$East,
          North = input$North,
          dateFormat = dateFormat(),
          prepostPeriod = input$prepostPeriod,
          EPSGcode =EPSGcode(),
          buffer = input$buffer,
          count = input$count,
          indID = input$indID,
          lastClustersFile = lastClustersFile(),
          minute_diff = input$minute_diff,
          onlyClusters = input$onlyClusters,
          oldclusters = input$oldclusters,
          UTM_zone = UTM_zone()
        )
      }

    output

    }
  )

  status <- reactive({
    cluster_list <- cluster_list()
    cluster_list$status
  })


  output$status <- renderText({status()})



  #load latest file directly
  shinyFileChoose(input, 'latestfile', roots = volumes(), filetypes = c("shp"))

  latestfile <- reactive(input$latestfile)


  latestfile_path <- reactive({
    as.character(parseFilePaths(volumes,latestfile())$datapath)
  })

  output$latestfile_path <- renderPrint(if(length(latestfile_path()>0)){latestfile_path()})

  latestfile2 <- reactive({
    read_sf(latestfile_path())
  }
  )



  #extract the clusters_sf out of the list
  Clusters_sf <- reactive({
    if (input$doit) {
      cluster_list <- cluster_list()
      cluster_list$Clusters_sf
    } else if (length(latestfile_path()>0)) {
      latestfile2()
    }
  })


  #create reactive values to store the edited data

  Clusters_sf_table <- reactiveValues(data = NULL)

  #fill the reactuve value with the data
  observe({
    Clusters_sf_table$data <- Clusters_sf()
  }


  )


  #if the analysis is run, render the table

  output$clustersTable <- DT::renderDataTable({
  if (input$extraColumns == FALSE| input$onlyClusters == TRUE) {
    Clusters_sf_table$data %>%
      #st_drop_geometry %>%
      DT::datatable(
        colnames = c("Animal ID" = "ID",
                     "Cluster ID" = "ClusID",
                     "Number of GPS locations" = "sum",
                     "Percent of time spent at the location" = "prec_time",
                     "Number of GPS locations within/outside of the cluster" = "inout",
                     "Classification of clusters by GPS locations" = "ratio",
                     "First date visited" = "date_min",
                     "Last date visited" = "date_max",
                     "State" = "State",
                     "Date done" = "Done",
                     "Fieldworker" = "Worker",
                     "Notes" = "Notes",
                     "Centerpoint East" = "center_x",
                     "Centerpoint North" = "center_y"),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          server = FALSE,
          columnDefs = list(list(targets = '_all', className = 'dt-center'),
                            list(targets = c(3,4,5,15), visible = FALSE))), #hide multiple columns
        rownames = FALSE,
        extensions = 'Buttons',
        filter = list(position = "top"),
        selection = "single",
        editable = list(target = "cell", disable = list(columns =c(0:7,13, 13)))) #make columns after number 7 editable. define selectInput!


  } else {
    Clusters_sf_table$data %>%
      #st_drop_geometry %>%
    DT::datatable(
                  colnames = c("Animal ID" = "ID",
                               "Cluster ID" = "ClusID",
                               "Number of GPS locations" = "sum",
                               "Percent of time spent at the location" = "prec_time",
                               "Number of GPS locations within/outside of the cluster" = "inout",
                               "Classification of clusters by GPS locations" = "ratio",
                               "First date visited" = "date_min",
                               "Last date visited" = "date_max",
                               "State" = "State",
                               "Date done" = "Done",
                               "Fieldworker" = "Worker",
                               "Notes" = "Notes",
                               "Centerpoint East" = "center_x",
                               "Centerpoint North" = "center_y"),
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    server = FALSE,
                     columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                       list(targets = c(15), visible = FALSE))), #hide the geometry column
                  rownames = FALSE,
                  extensions = 'Buttons',
                  filter = list(position = "top"),
                  selection = "single",
                  editable = list(target = "cell", disable = list(columns =c(0:7,13, 13)))) #make columns after number 7 editable. define selectInput!


  }
 })




  # Update the reactive values when the DT is edited
  observeEvent(input$clustersTable_cell_edit, {
    info = input$clustersTable_cell_edit
    row = as.numeric(info$row)
    col = as.numeric(info$col)+1

    Clusters_sf_table$data[row, col] = info$value


  })



  # download the clusters file

  #extract settings file from output list
settings <- reactive({
    cluster_list <- cluster_list()
    cluster_list$settings
  })

# #fill the reactuve value with the data
# observe({
#   Clusters_sf_table$data <- settings()
# }

observeEvent(input$downloadClusters, {
    thedate <- strftime(Sys.Date(),"%y%m%d")


    if (".shp" %in% input$downloadClusters_buttons) {


      if(length(latestfile_path()>0)){
        fileName_clusters <- paste(dirname(latestfile_path()), "/Clusters_", input$indID, "_", thedate, ".shp", sep = "")
        st_write(Clusters_sf_table$data, fileName_clusters, append = FALSE)

        #should the settings table also be donwladed here???

      } else {
        fileName_clusters <- paste(dirname(file_path()), "/Clusters_", input$indID, "_", thedate, ".shp", sep = "")
        st_write(Clusters_sf_table$data, fileName_clusters, append = FALSE)

        fileName_settings <- paste(dirname(file_path()), "/Settings_", input$indID, "_", thedate, ".txt", sep = "")
        write.table(settings(), fileName_settings, sep = "=", col.names = FALSE)

      }


    }

    if (".xlsx" %in% input$downloadClusters_buttons) {

      Clusters_csv <- Clusters_sf_table$data[input[["clustersTable_rows_all"]],]

      #threshold 64000, because excel cannot write column larger than ~32000 characters. sf df has always textfile*2 -> 32000*2 = 64000
      threshold = 64000
      tolerance = 0.01

      if (sum(nchar(Clusters_csv$geometry) > threshold) > 0) {
        simplification_needed <- TRUE
      } else {
        simplification_needed <- FALSE
      }

      while (simplification_needed) {

        if (sum(nchar(Clusters_csv$geometry) > threshold) > 0) {

          Clusters_csv <- st_simplify(Clusters_csv, dTolerance = tolerance)
          tolerance = tolerance + 0.01

          } else {

          simplification_needed <- FALSE
        }
      }


      geometry <- st_as_text(Clusters_csv$geometry)

      Clusters_csv <- Clusters_csv %>%
        st_drop_geometry() %>%
        cbind(geometry)


      #############

      if(length(latestfile_path()>0)){
        fileName_clusters <- paste(dirname(latestfile_path()), "/Clusters_", input$indID, "_", thedate, ".xlsx", sep = "")
        #write.csv(Clusters_csv, fileName_clusters)
        write.xlsx(Clusters_csv, fileName_clusters)
      } else {
        fileName_clusters <- paste(dirname(file_path()), "/Clusters_", input$indID, "_", thedate, ".xlsx", sep = "")
        #write.csv(Clusters_csv, fileName_clusters)
        write.xlsx(Clusters_csv, fileName_clusters)
      }

    }

    if (".gpx" %in% input$downloadClusters_buttons) {
      ClusID  <- NULL

      Clusters_gpx <- Clusters_sf_table$data[input[["clustersTable_rows_all"]],] %>%
        st_cast("LINESTRING") %>%
        st_transform(4326) %>%
        dplyr::select(ClusID) %>%
        rename(name = ClusID)

      if(length(latestfile_path()>0)){
        fileName_clusters <- paste(dirname(latestfile_path()), "/GPXClusters_", input$indID, "_", thedate, ".gpx", sep = "")

        st_write(Clusters_gpx, fileName_clusters, dataset_options="GPX_USE_EXTENSIONS=yes",layer="tracks", driver = "GPX", append = FALSE)

      } else {
        fileName_clusters <- paste(dirname(file_path()), "/GPXClusters_", input$indID, "_", thedate, ".gpx", sep = "")

        st_write(Clusters_gpx, fileName_clusters, dataset_options="GPX_USE_EXTENSIONS=yes",layer="tracks", driver = "GPX", append = FALSE)
      }
    }

  })


  # #load raster file -> would be nice but probably slows down extremly
  # shinyFileChoose(input, 'rasterfile', roots = volumes(), filetypes = c("tif"))
  # rasterfile <- reactive(input$rasterfile)
  #
  #
  # rasterfile_path <- reactive({
  #   as.character(parseFilePaths(volumes,rasterfile())$datapath)
  # })
  #
  #
  # rasterfile2 <- reactive({
  #   if (length(rasterfile_path()>0)) {
  #   raster(rasterfile_path())
  #   } else{
  #   NULL
  #   }
  # }
  # )


  #plot the clusters table

  map <- eventReactive(
    input$go, {
      isolate(Clusters_sf_table$data)

      s <- input$clustersTable_rows_selected
      filter <- input$clustersTable_rows_all


      if (length(latestfile_path()>0)) {
        init <- tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "State of clusters",col = "State", alpha = 0.6, lwd = 8, palette = c("Done" = "green",
                                                                                  "New" = "red",
                                                                                  "GPS locations added" = "blue",
                                                                                  "Not done" = "orange"),
                  popup.vars = c("Individual ID" = "ID",
                                 "Cluster ID" = "ClusID",
                                 "Number of GPS locations" = "sum",
                                 "First date visited" = "date_min",
                                 "Last date visited" = "date_max",
                                 "State" = "State")) +

          tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "Events for clusters", col = "Event", lwd = 1, lty = "dashed", colorNA = NULL)  +

          tm_text(group = "Cluster IDs", "ClusID", size = 1.5, col = "black") +

          tmap_options(basemaps = c("Esri.WorldTopoMap", "Esri.WorldImagery"))

        if (length(s)>0) {
          UTM_zone <-  as.numeric(paste0("258", UTM_zone()))
          selection <- st_as_sf(Clusters_sf_table$data[s,], coords = c("center_x", "center_y"), crs = UTM_zone)
          init + tm_shape(selection) + tm_dots(size = 4, alpha = 0.5, col = "yellow")
        } else init


      } else{

        ts_num <- NULL

        cluster_list <- cluster_list()
        cluster_list$Join_sf <- cluster_list$Join_sf %>%
          mutate(ts_num = as.numeric(difftime(as.POSIXct(ts), min(cluster_list$Join_sf$ts), units = "days")),
                 num = ts_num/max(ts_num))

        last_position <- cluster_list$Join_sf %>%
          group_by(ID) %>%
          arrange(desc(ts)) %>%
          slice(1)


        init <- tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "State of clusters",col = "State", alpha = 0.6, lwd = 8, palette = c("Done" = "green",
                                                                                  "New" = "red",
                                                                                  "Points added" = "blue",
                                                                                  "Not done" = "orange"),
                  popup.vars = c("Cluster ID" = "ClusID",
                                 "Individual ID" = "ID",
                                 "Number of GPS locations" = "sum",
                                 "First date visited" = "date_min",
                                 "Last date visited" = "date_max",
                                 "State" = "State")) +

          tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "Events for clusters", col = "Event", lwd = 1, lty = "dashed", colorNA = NULL, palette = "Dark2")  +

          tm_text(group = "Cluster IDs", "ClusID", size = 1.5, col = "black") +

          tm_shape(cluster_list$Join_sf) +
          tm_dots(group = "GPS Locations", size = "num", scale = 0.5, col = as.factor("ID"), id = "ident", popup.vars = c("Point ID" = "ident",
                                                                                                                       "Time stamp" = "ts",
                                                                                                                       "ID" = "ID",
                                                                                                                       "Cluster ID" = "ClusID"),
                  palette = "Dark2",
                  legend.show = FALSE)+
          tm_shape(last_position, name = "Last Position") +
          tm_markers( popup.vars = c("Point ID" = "ident",
                                                             "Time stamp" = "ts",
                                                             "ID" = "ID")) +

          tm_shape(cluster_list$data_sf_traj) + tm_lines(group = "Track", col = as.factor("ID"), palette = "Dark2") +

          tmap_options(basemaps = c("Esri.WorldTopoMap", "Esri.WorldImagery"))


        if (length(s)>0) {
          UTM_zone <-  as.numeric(paste0("258", UTM_zone()))
          selection <- st_as_sf(Clusters_sf_table$data[s,], coords = c("center_x", "center_y"), crs =  UTM_zone)
          init + tm_shape(selection) + tm_dots(size = 4, alpha = 0.5, col = "yellow")
        } else init

      }





    })

  observeEvent(input$downloadMap, {

    thedate <- strftime(Sys.Date(),"%y%m%d")

    if(length(latestfile_path()>0)){
      fileName_map <- paste(dirname(latestfile_path()), "/Map_", input$indID, "_", thedate, ".html", sep = "")
      tmap_save(map(), fileName_map)

    } else {
      fileName_map <- paste(dirname(file_path()), "/Map_", input$indID, "_", thedate, ".html", sep = "")
      tmap_save(map(), fileName_map)
    }

  })



  output$clusterMap <- renderLeaflet({

    if (length(latestfile_path()>0)) {
      tmap_leaflet(map()) %>%
        hideGroup(c("Events for clusters", "Cluster IDs"))
    } else {
      tmap_leaflet(map()) %>%
        hideGroup(c("Events for clusters", "Cluster IDs", "GPS Locations", "Track"))
    }


  })





  Join_sf <- reactive({

    cluster_list <- cluster_list()

    cluster_list$Join_sf

  })


  #create reactive values to store the edited data

  Join_sf_table <- reactiveValues(data = NULL)


  #fill the reactuve value with the data
  observe({
    Join_sf_table$data <- Join_sf()
  }


  )



  output$pointsTable <- DT::renderDataTable({

    req(input$doit)

    datatable(Join_sf_table$data,
              colnames = c("Point ID" = "ident",
                           "Animal ID" = "ID",
                           "Cluster ID" = "ClusID",
                           "Time stamp" = "ts",
                           "East" = "x",
                           "North" = "y"),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                server = FALSE,
                columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                  list(targets = c(1,3,4,6,7,8,9,10), visible = FALSE))),
              rownames = FALSE,
              extensions = 'Buttons',
              filter = list(position = "top"))
  })


  observeEvent(input$downloadPoints, {

    thedate <- strftime(Sys.Date(),"%y%m%d")

    if (".shp" %in% input$downloadPoints_buttons) {

      fileName_points <- paste(dirname(file_path()), "/Points_", input$indID, "_", thedate, ".shp", sep = "")
      st_write(Join_sf_table$data, fileName_points, append = FALSE)

    }

    if (".xlsx" %in% input$downloadPoints_buttons) {
      Points_csv <- Join_sf_table$data[input[["clustersTable_rows_all"]],]

      geometry <- st_as_text(Points_csv$geometry)

      Points_csv <- Points_csv %>%
        st_drop_geometry() %>%
        cbind(geometry)


      fileName_points <- paste(dirname(file_path()), "/Points_", input$indID, "_", thedate, ".xlsx", sep = "")
      write.xlsx(Points_csv, fileName_points)

    }


    if (".gpx" %in% input$downloadPoints_buttons) {

      ident <- NULL

      fileName_points <- paste(dirname(file_path()), "/GPXPoints_", input$indID, "_", thedate, ".gpx", sep = "")

      Join_gpx <- Join_sf_table$data[input[["pointsTable_rows_all"]],] %>%
        st_transform(4326) %>%
        dplyr::select(ident) %>%
        rename(name = ident)

      st_write(Join_gpx, fileName_points, dataset_options = "GPX_USE_EXTENSIONS=YES", layer="waypoints", driver = "GPX", append = FALSE)


      # Join_gpx <- cluster_list$Join_sf %>%
      #   st_transform(4326) %>%
      #   mutate(latitude = st_coordinates(.)[,1],
      #          longitude = st_coordinates(.)[,2])%>%
      #   dplyr::select(ident, latitude, longitude) %>%
      #   st_drop_geometry()
      #
      # writeGPX(Join_gpx, filename = "C:/Users/johan/Documents/ClusterApp Data/Wolf/gpx_points.gpx", type = "w")
      # st_write(Join_gpx, "C:/Users/johan/Documents/ClusterApp Data/Wolf/gpx_points.gpx", dataset_options = "GPX_USE_EXTENSIONS=YES", layer="waypoints", driver = "GPX", append = FALSE)



    }

  })





}


