#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' #' @import shinyFiles
#' @importFrom shiny actionButton br checkboxGroupInput checkboxInput column fluidPage dateRangeInput fluidRow h5 h6 hr navlistPanel numericInput radioButtons span tabPanel tabsetPanel tagList textInput textOutput titlePanel uiOutput verbatimTextOutput
#' @importFrom DT dataTableOutput
#' @importFrom leaflet leafletOutput
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#'
#'
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    fluidPage(
      theme = bslib::bs_theme(bootswatch = "superhero"),

      # tags$head(
      #   tags$link(rel = "icon", type = "image/png", href = "inst/app/www/favicon.png")
      # ),
      # titlePanel("ClusterApp: A Shiny R application to guide cluster studies based on GPS data"),


      # Application title
      #titlePanel("ClusterApp: A Shiny R application to guide cluster studies based on GPS data", windowTitle = tags$img(src = "www/favicon.png")),

     # navbarPage(title = "ClusterApp", windowTitle = tags$img(src = "inst/app/www/favicon.png", width = 30, height = 30)),

      navlistPanel(

        widths = c(3,9),

        tabPanel("Upload GPS data",
                 tabsetPanel(
                   tabPanel("Settings",
                            fluidRow(

                              # tags$head(
                              #   tags$style(
                              #     HTML("label{float:left;}", ".radio-inline, .checkbox-inline {padding-left: 20px;}"))),

                              column(6, br(), radioButtons("demo_data", "File upload: ", choiceNames = list("Manual upload", "Demo data wolf", "Demo data bears"),
                                                     choiceValues = list("Manual upload", "Demo data wolf", "Demo data bears"), selected="Manual upload",inline=TRUE),
                                     br(),
                                      shinyFilesButton("GISfile",
                                                      label = tags$span("Upload the original GPS file (.csv or .shp) here:",
                                                                        tags$i(
                                                                          class = "glyphicon glyphicon-info-sign",
                                                                          style = "color:#b20019;",
                                                                            title = "Find the file that includes your newest GPS data for the individual(s) you are monitoring. It makes sense to have a seperate folder for each study and always copy the GPS files here. This way, you will have a folder per monitored individual(s) that will contain all important files for the cluster analysis that was performed."
                                                                        )),
                                                      title = "Find the original GPS data for the individual(s).",
                                                      multiple = FALSE),
                                     br(),br(),
                                     radioButtons("separator","CSV Separator:", choiceNames = list("tab", ";",",",":"), choiceValues = list("\t",";",",",":"), selected=",",inline=TRUE)),
                              column(6,br(), br(), br(),textInput("indID", tags$span("Give the individual or group of individuals a label:",
                                                                     tags$i(
                                                                       class = "glyphicon glyphicon-info-sign",
                                                                       style = "color:#b20019;",
                                                                       title = "Here you should enter a unique label for this analysis, which will be an identifier for this cluster analysis of the individual(s) that are being studied. It makes sense to use the same label as the folder name your data is located in."
                                                                     )), value = "label"))),


                            br(), verbatimTextOutput("file_path",placeholder = TRUE),
                            #textOutput("fileStatus"),

                            h5("Select the appropriate column names here:"),

                            fluidRow(
                              column(6,
                                pickerInput(
                                  inputId = "ID",
                                  label = "Animal ID(s) as character",
                                  choices = colnames(NULL)),
                                pickerInput(
                                  inputId = "LMT_Date",
                                  label = "Timestamp as Date format",
                                  choices = colnames(NULL)),
                                pickerInput(
                                  inputId = "East",
                                  label = "Easting (Latitude) as numeric",
                                  choices = colnames(NULL)),
                                pickerInput(
                                  inputId = "North",
                                  label = "Northing (Longitude) as numeric",
                                  choices = colnames(NULL))),

                              column(6,
                                     br(),

                                     textInput("dateFormat", "If necessary, adjust the format of your date:", value = "%Y-%m-%d %H:%M:%S"),

                                     textInput("EPSG",
                                               tags$span("Additionally enter the EPSG code of the coordinate system, your data is in:",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#b20019;",
                                                           title = "Data can be in different coordinate systems. The most common coordinate system from WRAM Export is WGS84 (EPSG: 4326). Coordinate columns are usually named Longitude (Northing) and Latitude (Easting). The EPSG code is a unique code for each coordinate system."
                                                         ),
                                                         tags$a(href="https://epsg.io/", "Find the codes here (connection to browser necessary).", target="_blank")),
                                               value = "4326"),
                                     selectInput("UTM_zone",
                                               tags$span("The output will be in the UTM format. Please enter the right zone:",
                                                         tags$i(
                                                           class = "glyphicon glyphicon-info-sign",
                                                           style = "color:#b20019;",
                                                           title = "UTM coordinate system is separated into zones over the world. Please find the appropriate zone in which your data lies. Be aware that this should be the same zone as a previous cluster shape, that might have been downloaded before."
                                                         ),
                                                         tags$a(href="https://www.xmswiki.com/wiki/UTM_Coordinate_System", "Find the zones here (connection to browser necessary).", target="_blank")),
                                               choices = 1:60)
                                     )
                              ),
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
                          # textInput("indID", tags$span("Give the individual or group of individuals a label:",
                          #                              tags$i(
                          #                                class = "glyphicon glyphicon-info-sign",
                          #                                style = "color:#b20019;",
                          #                                title = "Here you should enter a unique label for this analysis, which will be an identifier for this cluster analysis of the individual(s) that are being studied. It makes sense to use the same label as the folder name your data is located in."
                          #                              )), value = "label"),


                          numericInput("buffer",
                                       tags$span("Set the buffer size in meters:",
                                                 tags$i(
                                                   class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#b20019;",
                                                   title = "The size of the buffer corresponds to the radius of the buffer that is put around each GPS location. Combining buffers are the basis for GPS location clusters to emerge. Setting the size of the buffer depends on your question, the larger you set the radius, the more GPS location clusters will develop and possibly combine to large areas."
                                                 )), value = NA),

                          numericInput("count",
                                       tags$span("Set the number of GPS locations it needs to be a buffer:",
                                                 tags$i(
                                                   class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#b20019;",
                                                   title = "The number of locations within a polygon define when it is called in cluster. Next to the buffer size, this amount depends on your question. The higher the number of GPS locations within a polygon, the less GPS location clusters will develop."
                                                 )), value = NA),

                          shiny::dateRangeInput("intensivePeriod", label = tags$span("Define the study period:",
                                                                              tags$i(
                                                                                class = "glyphicon glyphicon-info-sign",
                                                                                style = "color:#b20019;",
                                                                                title = "Define the time frame that you are interested in.  Usually there is a time frame where the GPS sent positions more intensivly (e.g. every hour in comparison to usually only send every three hours) or the individual is monitored over a set number of weeks."
                                                                              ))), #tick if you need a time frame around the intensive period
                          numericInput("prepostPeriod", label = tags$span("Enter the number of days that will be added before and after the study period:",
                                                                          tags$i(
                                                                            class = "glyphicon glyphicon-info-sign",
                                                                            style = "color:#b20019;",
                                                                            title = "For some analysis you might want to add a time frame around the study periods of a number of days. Points within this time frame are still taken into account for GPS location clusters, if at least one point within was also during the study period."
                                                                          )), value = 0)
                   ),
                   column(4, offset = 1,
                          h5(tags$span("Summary for optional time stamp filtering:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "color:#b20019;",
                                         title = "Here it is optional to set the time difference it needs between GPS locations in minutes. If nothing is filled in all data will be used for the cluster analysis and the mean time frame is taken as a difference value for the calculations of 'Percent of time spent at the location.'"
                                       ))),
                          br(),
                          verbatimTextOutput("minute_diff_summary", placeholder = TRUE),
                          numericInput("minute_diff",
                                       tags$span("Set the time difference needed between GPS locations in minutes:",
                                                 tags$i(
                                                   class = "glyphicon glyphicon-info-sign",
                                                   style = "color:#b20019;",
                                                   title = "In the output above, you see the summary of time differences in your data. It makes sense to set your time difference at the approximate mean of your data, to use as many locations possible for the analysis without inflating GPS location clusters by GPS data that were taken shortly after one another."
                                                 )),
                                       value = NA),
                          br(), br(),
                          h6("Should the columns related to the time spent and the number of points within/outside of the cluster be displayed in the cluster table?"),
                          checkboxInput("extraColumns", label = "", value = FALSE),
                          br(),
                          h6("Should only GPS location clusters with consecutive GPS locations be developed?"),
                          checkboxInput("onlyClusters", label = "", value = FALSE)),

                   column(4, offset = 1,
                          h5(tags$span("Some additional info, if you already have done an analysis before:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "color:#b20019;",
                                         title = "Here the path to the latest cluster shapefile (.shp) appears automatically, if this one is saved in the same folder as your input GPS file and has the same label."
                                       ))),
                          br(),

                          verbatimTextOutput("file_path_last",placeholder = TRUE),

                          shinyFilesButton("latestfile_manual",
                                           label = tags$span("Optionally load a latest file manually",
                                                             tags$i(
                                                               class = "glyphicon glyphicon-info-sign",
                                                               style = "color:#b20019;",
                                                               title = "Manually find a latest cluster file to be used in the analysis (allowed formats are .shp or .xlsx). This file has to have the columns: ID, ClusID, sum, prec_time, date_min, date_max,  State , Event,  Done, Worker, center_x, center_y, geometry. Manual changes to the file within the columns State , Event,  Done and Worker will be used.")),
                                           title = "Manually find a latest cluster file to be used.",
                                           multiple = FALSE),

                          br(),
                          br(),

                          h6("Should old GPS location clusters automatically be marked as done?"),
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
                   tabPanel(tags$span("GPS location clusters (GLC)",
                                      tags$i(
                                        class = "glyphicon glyphicon-info-sign",
                                        style = "color:#b20019;",
                                        title = "The GLC table shows all GLC that were formed by the number of GPS locations that were buffered and grew together. The data can be filtered in the top row and only this data is downloaded when choosing the formats .csv and .gpx. The columns State, Event, Date done, Fieldworker and Notes can be edited. These edits will be saved when downloaded."
                                      )),
                            fluidRow(

                              DT::dataTableOutput('clustersTable'),
                              column(4,checkboxGroupInput("downloadClusters_buttons", "Choose output format:", choiceNames =  list( ".shp",".xlsx", ".gpx"), selected = c(".shp"), choiceValues = list( ".shp",".xlsx", ".gpx"), inline = TRUE)),

                              column(4, br(), actionButton("downloadClusters", label =  tags$span("Download cluster table.",
                                                                                                  tags$i(
                                                                                                    class = "glyphicon glyphicon-info-sign",
                                                                                                    style = "color:#b20019;",
                                                                                                    title = "You have to download the shapefile of this cluster analysis, if this should be used in the following anaylsis as a latest cluster analysis file. This file will be downloaded in the coordinate system UTM and the specified zone. Downloading .xlsx or.gpx files are optional. GPX will be in WGS84."
                                                                                                  )))),
                              #h5("Plotting the data, a marker shows the last position of the animal(s). You have the option to additionally select layers to display the events of the clusters and written cluster IDs, as well as GPS points and the track for the individuals. The points increase in size the more recent the points have been made."),
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

                              actionButton("go",label = tags$span("Plot data",
                                                                  tags$i(
                                                                    class = "glyphicon glyphicon-info-sign",
                                                                    style = "color:#b20019;",
                                                                    title = "Plotting the data, a marker shows the last position of the animal(s). You have the option to additionally select layers to display the events of the GPS location clusters and written cluster IDs, as well as GPS locations and the track for the individuals. The points increase in size the more recent the GPS locations have been made."
                                                                  )), class = "btn-success", width = '100%'),
                              leafletOutput('clusterMap'),
                              actionButton("downloadMap", label = "Download interactive map as html."))),


                   tabPanel(tags$span("GPS data",
                                      tags$i(
                                        class = "glyphicon glyphicon-info-sign",
                                        style = "color:#b20019;",
                                        title = "The GPS datatable shows all GPS locations, that were used for the cluster analysis. The point ID is a combination for easier identification in the field and is a combination of the individual ID, the GLC it belongs to or SP for single point, the month, day and hour of the GPS location."
                                      )),
                            fluidRow(
                              DT::dataTableOutput("pointsTable"),
                              column(4, checkboxGroupInput("downloadPoints_buttons", "Choose output format:", choiceNames =  list( ".shp",".xlsx", ".gpx"), choiceValues = list( ".shp",".xlsx", ".gpx"), inline = TRUE)),
                              column(4, br(), actionButton("downloadPoints", label =  tags$span("Download GPS datatable.",
                                                                                                tags$i(
                                                                                                  class = "glyphicon glyphicon-info-sign",
                                                                                                  style = "color:#b20019;",
                                                                                                  title = "Downloading this file is useful for uploading it on your GPS, it is not necessary for any further analysis. The shapefile and excel file will be downloaded in the coordinate system UTM and the specified zone, while the GPX file is downloaded in WGS84."
                                                                                                ))))))
                 )
        )
      ),

      tags$div(
        style = "text-align: center; margin-top: 20px;",
        HTML("This app is developed by <a href='mailto:johanna@maertz.eu'>Johanna Maertz</a>. Any questions, feedback, or ideas to improve it, please feel free to reach out (last updated 16.04.2024).")
      )
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
