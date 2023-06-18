#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @import shinyFiles
#' @import shinyWidgets
#' @importFrom dplyr filter mutate group_by arrange desc select slice rename lag
#' @importFrom DT datatable renderDataTable
#' @importFrom leaflet hideGroup renderLeaflet
#' @importFrom foreign read.dbf
#' @importFrom readr parse_number read_delim
#' @importFrom sf read_sf st_as_sf st_cast st_crs st_drop_geometry st_transform st_write
#' @importFrom tmap tm_dots tm_fill tm_lines tm_markers tm_shape tm_text tmap_leaflet tmap_options tmap_save
#' @importFrom utils str write.csv
#' @noRd
#'
#'
app_server <- function(input, output, session) {



  ## Dateipfad
  volumes = getVolumes()
  shinyFileChoose(input, 'GISfile', roots = volumes(), filetypes = c("dbf", "csv", "shp"))

  GISfile <- reactive(input$GISfile)


  file_path <- reactive({
    as.character(parseFilePaths(volumes,GISfile())$datapath)
  })


  output$file_path <- renderPrint(if(length(file_path()>0)){
    file_path()}
    else {
      "Please select a file!"
    })


  file <- reactive({
    if(sum(strsplit(basename(file_path()), split="\\.")[[1]][-1] == "csv") == TRUE){
      read_delim(file_path(), delim = input$separator, escape_double = FALSE, trim_ws = TRUE)

    }else if(sum(strsplit(basename(file_path()), split="\\.")[[1]][-1] == "dbf") == TRUE){
      read.dbf(file_path(), as.is = FALSE)

    } else if (sum(strsplit(basename(file_path()), split="\\.")[[1]][-1] == "shp") == TRUE){
      read_sf(file_path())
    } else {
      NULL
    }
  })






  output$pickerID <- renderUI(if(length(file_path()>0)){
    pickerInput(inputId = 'ID',
                label = 'ID',
                choices = colnames(file()))
  })

  output$pickerLMT_Date <- renderUI(if(length(file_path()>0)){
    pickerInput(inputId = 'LMT_Date',
                label = 'LMT_Date',
                choices = colnames(file()))
  })

  output$pickerEast <- renderUI(if(length(file_path()>0)){
    pickerInput(inputId = 'East',
                label = 'East (Latitude)',
                choices = colnames(file()))
  })
  output$pickerNorth <- renderUI(if(length(file_path()>0)){
    pickerInput(inputId = 'North',
                label = 'North (Longitude)',
                choices = colnames(file()))
  })



  output$file_str <- renderPrint(if(length(file_path()>0)){
    str(file())})


  output$file_summary <- renderPrint(if(length(file_path()>0)){
    summary(file())})



  EPSGcode <- reactive({st_crs(as.numeric(input$EPSG))})
  dateFormat <- reactive({input$dateFormat})


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

    data$diff_min

  }
  )

  output$minute_diff_summary <- renderPrint(if(length(file_path()>0)){

    summary(minute_diff_summary())

  })



  #search in this folder for an older cluster file from that individual
  lastClustersFile <- reactive(ifelse((sum(grepl(paste0("Clusters_", input$indID),
                                                 list.files(dirname(file_path()))))>0) &
                                        (sum(grepl(paste0("\\bClusters_", input$indID,"_",
                                                          stringr::str_sub(list.files(dirname(file_path()),
                                                                                      pattern = paste0("Clusters_", input$indID)), -10)  %>%
                                                            readr::parse_number() %>%
                                                            max(),".shp\\b"), list.files(dirname(file_path())))>0)),

                                      paste(dirname(file_path()),"/Clusters_", input$indID,"_",
                                            stringr::str_sub(list.files(dirname(file_path()),
                                                                        pattern = paste0("Clusters_", input$indID)), -10)  %>%
                                              readr::parse_number() %>%
                                              max(),".shp" ,sep = ""), "No latest cluster file.")
  )



  output$file_path_last <- renderPrint(lastClustersFile())




  cluster_list <- eventReactive(
    input$doit, {

    output <- cluster_analysis(
        intensive.start = input$intensivePeriod[1],
        intensive.end = input$intensivePeriod[2],
        datapoints = file(),
        ID = input$ID,
        LMT_Date = input$LMT_Date,
        East = input$East,
        North = input$North,
        dateFormat = dateFormat(),
        prepostPeriod = 0, #input$prepostPeriod
        EPSGcode = EPSGcode(),
        buffer = input$buffer,
        count = input$count,
        indID = input$indID,
        lastClustersFile = lastClustersFile(),
        minute_diff = input$minute_diff,
        oldclusters = input$oldclusters
      )

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

  output$clustersTable <- renderDataTable({


    DT::datatable(Clusters_sf_table$data,
                  colnames = c("Individual ID" = "ID",
                               "Cluster ID" = "ClusID",
                               "Number of points" = "sum",
                               "Percent of time spent at the location" = "prec_time",
                               "First date visited" = "date_min",
                               "Last date visited" = "date_max",
                               "State" = "State",
                               "Date done" = "Done",
                               "Fieldworker" = "Worker",
                               "Centerpoint East" = "center_x",
                               "Centerpoint North" = "center_y"),
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    server = FALSE,
                    columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                      list(targets = c(12), visible = FALSE))), #hide the geometry column
                  rownames = FALSE,
                  extensions = 'Buttons',
                  filter = list(position = "top"),
                  selection = "single",
                  editable = list(target = "cell", disable = list(columns =c(0:5,10, 11)))) #make columns after number 4 editable. define selectInput!
  })




  # Update the reactive values when the DT is edited
  observeEvent(input$clustersTable_cell_edit, {
    info = input$clustersTable_cell_edit
    row = as.numeric(info$row)
    col = as.numeric(info$col)+1

    Clusters_sf_table$data[row, col] = info$value


  })



  # download the clusters file

  observeEvent(input$downloadClusters, {
    thedate <- strftime(Sys.Date(),"%y%m%d")

    if (".shp" %in% input$downloadClusters_buttons) {


      if(length(latestfile_path()>0)){
        fileName_clusters <- paste(dirname(latestfile_path()), "/Clusters_", input$indID, "_", thedate, ".shp", sep = "")
        st_write(Clusters_sf_table$data, fileName_clusters, append = FALSE)

      } else {
        fileName_clusters <- paste(dirname(file_path()), "/Clusters_", input$indID, "_", thedate, ".shp", sep = "")
        st_write(Clusters_sf_table$data, fileName_clusters, append = FALSE)
      }


    }

    if (".csv" %in% input$downloadClusters_buttons) {
      Clusters_csv <- Clusters_sf_table$data[input[["clustersTable_rows_all"]],] %>%
        st_drop_geometry()



      if(length(latestfile_path()>0)){
        fileName_clusters <- paste(dirname(latestfile_path()), "/CsvClusters_", input$indID, "_", thedate, ".csv", sep = "")
        write.csv(Clusters_csv, fileName_clusters)
      } else {
        fileName_clusters <- paste(dirname(file_path()), "/CsvClusters_", input$indID, "_", thedate, ".csv", sep = "")
        write.csv(Clusters_csv, fileName_clusters)
      }

    }

    if (".gpx" %in% input$downloadClusters_buttons) {
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
          tm_fill(group = "State of clusters",col = "State", lwd = 8, palette = c("Done" = "green",
                                                                                  "New" = "red",
                                                                                  "Points added" = "blue",
                                                                                  "Not done" = "orange"),
                  popup.vars = c("Individual ID" = "ID",
                                 "Cluster ID" = "ClusID",
                                 "Number of points" = "sum",
                                 "First date visited" = "date_min",
                                 "Last date visited" = "date_max",
                                 "State" = "State")) +

          tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "Events for clusters", col = "Event", lwd = 1, lty = "dashed", colorNA = NULL)  +

          tm_text(group = "Cluster IDs", "ClusID", size = 1.5, col = "black") +

          tmap_options(basemaps = c("Esri.WorldTopoMap", "Esri.WorldImagery"))

        if (length(s)>0) {
          selection <- st_as_sf(Clusters_sf_table$data[s,], coords = c("center_x", "center_y"), crs == 25833)
          init + tm_shape(selection) + tm_dots(size = 4, alpha = 0.5, col = "yellow")
        } else init


      } else{

        cluster_list <- cluster_list()
        cluster_list$Join_sf <- cluster_list$Join_sf %>%
          mutate(ts_num = as.numeric(difftime(as.POSIXct(ts), min(cluster_list$Join_sf$ts), units = "days")),
                 num = ts_num/max(ts_num))

        last_position <- cluster_list$Join_sf %>%
          group_by(ID) %>%
          arrange(desc(ts)) %>%
          slice(1)


        init <- tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "State of clusters",col = "State", lwd = 8, palette = c("Done" = "green",
                                                                                  "New" = "red",
                                                                                  "Points added" = "blue",
                                                                                  "Not done" = "orange"),
                  popup.vars = c("Individual ID" = "ID",
                                 "Cluster ID" = "ClusID",
                                 "Number of points" = "sum",
                                 "First date visited" = "date_min",
                                 "Last date visited" = "date_max",
                                 "State" = "State")) +

          tm_shape(Clusters_sf_table$data[filter,]) +
          tm_fill(group = "Events for clusters", col = "Event", lwd = 1, lty = "dashed", colorNA = NULL)  +

          tm_text(group = "Cluster IDs", "ClusID", size = 1.5, col = "black") +

          tm_shape(cluster_list$Join_sf) +
          tm_dots(group = "GPS Points", size = "num", scale = 0.5, col = as.factor("ID"), id = "ident", popup.vars = c("Point ID" = "ident",
                                                                                                                       "Time stamp" = "ts",
                                                                                                                       "ID" = "ID",
                                                                                                                       "Cluster ID" = "ClusID"), legend.show = FALSE)+
          tm_shape(last_position) +
          tm_markers(group = "Last Position", popup.vars = c("Point ID" = "ident",
                                                             "Time stamp" = "ts",
                                                             "ID" = "ID")) +

          tm_shape(cluster_list$data_sf_traj) + tm_lines(group = "Track", col = as.factor("ID")) + #highlight first and last position

          tmap_options(basemaps = c("Esri.WorldTopoMap", "Esri.WorldImagery"))


        if (length(s)>0) {
          selection <- st_as_sf(Clusters_sf_table$data[s,], coords = c("center_x", "center_y"), crs == 25833)
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
      tmap_leaflet(map(), in.shiny = TRUE) %>%
        hideGroup(c("Events for clusters", "Cluster IDs"))
    } else {
      tmap_leaflet(map(), in.shiny = TRUE) %>%
        hideGroup(c("Events for clusters", "Cluster IDs", "GPS Points", "Track"))
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



  output$pointsTable <- renderDataTable({

    req(input$doit)

    datatable(Join_sf_table$data,
              colnames = c("Point ID" = "ident",
                           "Individual ID" = "ID",
                           "Time stamp" = "ts",
                           "East" = "x",
                           "North" = "y"),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                server = FALSE,
                columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                  list(targets = c(1,3,4,6,7,8,9,10,11), visible = FALSE))),
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

    if (".csv" %in% input$downloadPoints_buttons) {
      Points_csv <- Join_sf_table$data[input[["clustersTable_rows_all"]],] %>%
        st_drop_geometry()


      fileName_points <- paste(dirname(file_path()), "/CsvPoints_", input$indID, "_", thedate, ".csv", sep = "")
      write.csv(Points_csv, fileName_points)

    }


    if (".gpx" %in% input$downloadPoints_buttons) {

      fileName_points <- paste(dirname(file_path()), "/GPXPoints_", input$indID, "_", thedate, ".gpx", sep = "")

      Join_gpx <- Join_sf_table$data[input[["pointsTable_rows_all"]],] %>%
        st_transform(4326) %>%
        dplyr::select(ident, ts) %>%
        rename(name = ident)

      st_write(Join_gpx, fileName_points, dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints", driver = "GPX", append = FALSE)

    }

  })





}


