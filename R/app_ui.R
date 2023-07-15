#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' #' @import shinyFiles
#' @importFrom shiny actionButton br checkboxGroupInput checkboxInput column fluidPage dateRangeInput fluidRow h5 h6 hr navlistPanel numericInput radioButtons span tabPanel tabsetPanel tagList textInput textOutput titlePanel uiOutput verbatimTextOutput
#' @importFrom DT dataTableOutput
#' @importFrom leaflet leafletOutput
#' @noRd
#'
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    fluidPage(

      theme = bslib::bs_theme(bootswatch = "superhero"),
      # Application title
      titlePanel("Cluster Analysis"),

      #navbarPage(title = "ClusterApp", windowTitle = tags$img(src = "favicon.png", width = 30, height = 30)),

      navlistPanel(

        widths = c(3,9),

        tabPanel("Upload GPS data",
                 tabsetPanel(
                   tabPanel("Settings",
                            fluidRow(
                              column(4, br(),
                                     shinyFilesButton("GISfile",
                                                      label = tags$span("Upload the original GPS file (.csv, .shp or .dbf) here:",
                                                                        tags$i(
                                                                          class = "glyphicon glyphicon-info-sign",
                                                                          style = "color:#b20019;",
                                                                            title = "Find the file that includes your newest GPS data for the individual you are monitoring. It makes sense to have a seperate folder for each individual and always copy the GPS files here. This way, you will have a folder per monitored individual that will contain all important files for the cluster analysis that was performed."
                                                                        )),
                                                      title = "Find the original GPS points for the individual.",
                                                      multiple = FALSE)),
                              column(2, radioButtons("separator","CSV Separator: ", choiceNames = list("tab", ";",",",":"), choiceValues = list("\t",";",",",":"), selected="\t",inline=TRUE))
                            ),


                            verbatimTextOutput("file_path",placeholder = TRUE),
                            #textOutput("fileStatus"),

                            h5("The file needs the columns:"),
                            h6("The column with the individuals ID (or multiple IDs) in character format,
            Timestamp either in character or date format,
            East and North coordinates for each point (in numeric)."),
                            h6("Select the appropriate column names here:"),

                            fluidRow(
                              column(6, uiOutput("pickerID"),
                                     uiOutput("pickerLMT_Date"),
                                     uiOutput("pickerEast"),
                                     uiOutput("pickerNorth")),
                              column(6,

                                     #textOutput("columnStatus"),
                                     br(),
                                     br(),


                                     textInput("dateFormat", "If necessary, adjust the format of your date:", value = "%Y-%m-%d %H:%M:%S"),

                                     textInput("EPSG",
                                               tags$span("Additionally enter the EPSG code of the coordinate system, your data is in:",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#b20019;",
                                                           title = "Data can be in different coordinate systems. The most common coordinate system from WRAM Export is WGS84 (EPSG: 4326). Coordinate columns are usually named Longitude (Northing) and Latitude (Easting). The EPSG code is a unique code for each coordinate system."
                                                         ),
                                                         tags$a(href="https://epsg.io/", "Find the codes here.")),
                                               value = "4326"),
                                     textInput("UTM_zone",
                                               tags$span("The output will be in the UTM format. Please enter the right zone:",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#b20019;",
                                                           title = "UTM coordinate system is separated into zones over the world. Please find the appropriate zone in which your data lies. Be aware that this should be the same zone as a previous cluster shape, that might have been downloaded before."
                                                         ),
                                                         tags$a(href="https://www.xmswiki.com/wiki/UTM_Coordinate_System", "Find the zones here.")),
                                               value = "33"))),



                   ),
                   tabPanel("Data",

                            verbatimTextOutput("file_str",placeholder = TRUE)
                   ),

                   tabPanel("Data Summary",

                            verbatimTextOutput("file_summary", placeholder = TRUE)
                   )
                 )
        ),

        tabPanel("Adjust Cluster Analysis Parameters",
                 fluidRow(
                   column(2,
                          textInput("indID", tags$span("Give the individual or group of individuals a unique ID:",
                                                       tags$i(
                                                         class = "glyphicon glyphicon-info-sign",
                                                         style = "color:#b20019;",
                                                         title = "Within the first step, you have chosen the column which includes the individual ID/IDs. Here you should enter a unique identifier for this analysis. If you are only looking at one individual, you can choose for the same ID as in the column. If you are looking at multiple individuals at the same time, give them a group name. It makes sense to use the same name as the folder name your data is located in."
                                                       )), value = "ID"),


                          numericInput("buffer",
                                       tags$span("Set the buffer size in meters:",
                                                 tags$i(
                                                   class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#b20019;",
                                                   title = "The size of the buffer corresponds to the radius of the buffer that is put around each GPS point. Combining buffers are the basis for clusters to emerge. Setting the size of the buffer depends on your question, the larger you set the radius, the more clusters will develop and possibly combine to large areas."
                                                 )), value = 50),

                          numericInput("count",
                                       tags$span("Set the number of points it needs to be a buffer:",
                                                 tags$i(
                                                   class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#b20019;",
                                                   title = "The number of points within a polygon define when it is called in cluster. Next to the buffer size, this amount depends on your question. The higher the number of points within a polygon, the less clusters will develop."
                                                 )),
                                       value = 2),

                          shiny::dateRangeInput("intensivePeriod", label = tags$span("Define the intensive period:",
                                                                              tags$i(
                                                                                class = "glyphicon glyphicon-info-sign",
                                                                                style = "color:#b20019;",
                                                                                title = "Define the time frame that you are interested in.  Usually there is a time frame where the GPS sent positions more intensivly (e.g. every hour in comparison to usually only send every three hours)."
                                                                              ))), #tick if you need a time frame around the intensive period
                          # numericInput("prepostPeriod", label = tags$span("Enter the number of days that will be added before and after the intensive period:",
                          #                                                 tags$i(
                          #                                                   class = "glyphicon glyphicon-info-sign",
                          #                                                   style = "color:#b20019;",
                          #                                                   title = "For some analysis you might want to add a time frame around the intensive periods of a number of days. Points within this time frame are still taken into account for clusters, if at least one point within was also during the intensive period."
                          #                                                 )), value = 0)
                   ),
                   column(4, offset = 1,
                          h5("Optional time stamp filtering:"),
                          h6("Here it is optional to set the time difference in minutes it needs between GPS locations. If nothing is filled in all GPS points will be used for the cluster analysis and the mean time frame is taken as a difference value for the calculations of 'Percent of time spent at the location.'"),
                          br(),
                          verbatimTextOutput("minute_diff_summary", placeholder = TRUE),
                          numericInput("minute_diff",
                                       tags$span("Set the time difference needed between GPS points in minutes:",
                                                 tags$i(
                                                   class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#b20019;",
                                                   title = "In the output above, you see the summary of time differences in your data. It makes sense to set your time difference at the approximate mean of your data, to use as many points possible for the analysis without inflating clusters by GPS points that were taken shortly after one another."
                                                 )),
                                       value = NA)),

                   column(4, offset = 1,
                          h5("Some additional info, if you already have done an analysis before:"),
                          h6("Here the path to the latest cluster file appears, if this one is saved in the same folder as your input GPS file and has the same ID."),
                          br(),
                          verbatimTextOutput("file_path_last",placeholder = TRUE),
                          h6("Should old clusters automatically be marked as done?"),
                          checkboxInput("oldclusters", label = "", value = FALSE)))
        ),


        tabPanel("Clusters Analysis Output",
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"),

                 actionButton("doit", "Perform cluster analysis", class = "btn-success"),
                 textOutput("status"),

                 br(),
                 shinyFilesButton("latestfile",
                                  label = tags$span("Alternativly upload a latest cluster file (.shp or .dbf) here:",
                                                    tags$i(
                                                      class = "glyphicon glyphicon-info-sign",
                                                      style = "color:#b20019;",
                                                      title = "If you only want to look and adjust a latest cluster file, witout having new data you can load it directly via this button. To save changes for later analysis, download the shapefile when you are done."
                                                    )),
                                  title = "Find a latest cluster file.",
                                  multiple = FALSE),
                 verbatimTextOutput("latestfile_path",placeholder = TRUE),
                 br(),

                 tabsetPanel(
                   tabPanel("Clusters",
                            fluidRow(
                              h5("The clusters table shows all clusters that were formed by the number of GPS points that were buffered and grew together. The data can be filtered in the top row and this data is downloaded with the .csv and .gpx file. The columns State, Event, Date done and Fieldworker can be edited. These edits will be saved when downloaded."),

                              DT::dataTableOutput('clustersTable'),
                              column(4,checkboxGroupInput("downloadClusters_buttons", "Choose output format:", choiceNames =  list( ".shp",".csv", ".gpx"), selected = c(".shp"), choiceValues = list( ".shp",".csv", ".gpx"), inline = TRUE)),

                              column(4, br(), actionButton("downloadClusters", label =  tags$span("Download cluster table.",
                                                                                                  tags$i(
                                                                                                    class = "glyphicon glyphicon-info-sign",
                                                                                                    style = "color:#b20019;",
                                                                                                    title = "You have to download the shapefile of this cluster analysis, if this should be used in the following anaylsis as a latest cluster analysis file. This file will be downloaded in the coordinate system UTM and the specified zone. Downloading .csv or.gpx files are optional. GPX will be in WGS84."
                                                                                                  )))),
                              h5("Plotting the data, a marker shows the last position of the animal(s). You have the option to additionally select layers to display the events of the clusters and written cluster IDs, as well as GPS points and the track for the individuals. The points increase in size the more recent the points have been made."),
                              # fluidRow(column(2, shinyFilesButton("rasterfile",
                              #                                     label = tags$span("tiff file :",
                              #                                                       tags$i(
                              #                                                         class = "glyphicon glyphicon-info-sign",
                              #                                                         style = "color:#b20019;",
                              #                                                         title = "Optional raster background."
                              #                                                       )),
                              #                                     title = "Optional raster background.",
                              #                                     multiple = FALSE)),
                              #
                              #          column(8, actionButton("go",label = "Plot Data", class = "btn-success", width = '100%'))),

                              actionButton("go",label = "Plot Data", class = "btn-success", width = '100%'),
                              leafletOutput('clusterMap'),
                              actionButton("downloadMap", label = "Download interactive map as html."))),


                   tabPanel("Points Table",
                            h5("The points datatable shows all GPS points, that were used for the cluster analysis. The point ID is a combination for easier identification in the field and is a combination of the ID, the cluster it belongs to or SP for single point, the month, day and hour the point was made."),
                            fluidRow(
                              DT::dataTableOutput("pointsTable"),
                              column(4, checkboxGroupInput("downloadPoints_buttons", "Choose output format:", choiceNames =  list( ".shp",".csv", ".gpx"), choiceValues = list( ".shp",".csv", ".gpx"), inline = TRUE)),
                              column(4, br(), actionButton("downloadPoints", label =  tags$span("Download points table.",
                                                                                                tags$i(
                                                                                                  class = "glyphicon glyphicon-info-sign",
                                                                                                  style = "color:#b20019;",
                                                                                                  title = "Downloading this file is useful for uploading it on your GPS, it is not necessary for any further analysis. The shapefile will be downloaded in the coordinate system UTM and the specified zone, while the GPX file is downloaded in WGS84."
                                                                                                ))))))
                 )
        )
      ),

      hr(),
      print("This app is developed by Johanna Maertz. Any questions, feedback or ideas to improve it mail to johanna@maertz.eu.")

    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ClusterApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
