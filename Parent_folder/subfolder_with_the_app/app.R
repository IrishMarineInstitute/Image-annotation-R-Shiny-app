
# install.packages(c("colorspace", "DT", "exifr", "hms", "jpeg", "magick", "plotrix", "raster", "shiny", "shinyBS", "shinyFiles", "shinyjs", "shinythemes", "shinyWidgets"))

library(colorspace)
library(DT)
library(exifr)
library(gtools)
library(hms)
library(jpeg)
library(magick)
library(plotrix)
library(raster)
library(shiny)
library(shinyBS)
library(shinyFiles)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

# Optional settings for working from other PCs in the same local network
    # options(shiny.port = 2345)
    # options(shiny.host = "10.10.1.40")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                shinyjs::useShinyjs(),
                # tags$head(tags$script(jss)),
    #             tags$head(tags$script(HTML("
    # // Enable navigation prompt
    # window.onbeforeunload = function() {
    #     return 'Are you sure you want to leave?';
    # };
    # "))),
             
    # Progress window setup:   
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             height: 100px;
             width: 800px;
             position:fixed;
             top: calc(50% - 50px);;
             left: calc(50% - 400px);;
             }
             "
        )
        )),

  # TOP ROW: 5 steps to start the app:
  fluidRow(
    column(1,
           uiOutput("surveyID")), # 1st select Survey
    column(1,
           uiOutput("counterID")), # 2nd select Counter
    column(1,
           uiOutput("stationID")), # 3rd select Station
    column(1,
           uiOutput("reviewer")), # 4th select Reviewer number
    column(1, br(),
           actionButton("start", "5th: Load stn")), # 4th press button to load station
    column(2,
           tags$h6("6th: press an arrow to start"), # 5th press arrow on keyboard to plot images on screen
           tags$h6("up=PAUSE ; right=PLAY ; left=REWIND")), # app driving instructions
    column(1,
           tags$h6("stills for this station"),
           verbatimTextOutput("jpgnumber")), # check number of images on the folder/station
    tags$style("#jpgnumber{height: 34px}"),
    column(3,
           tags$h6("Folder selected"),
           verbatimTextOutput("directorypath")), # check folder where the images come from (to check station number is correct)
    tags$style("#directorypath{font-size: 9px}"),

    # License
    column(1, actionButton("lic", "code & license")),
    tags$head(tags$style("#lic{font-size: 12px; text-align:center}")),
    tags$style("#lice{text-align:justify}"),
    bsModal("lice", HTML("Image annotation R Shiny app <br>
    Copyright (C) 2019. Mikel Aristegui*"), "lic", size = "large",
            HTML("

    Recommended citation:
    <p>Aristegui, M. (2019) Image annotation R Shiny app. Marine Institute. <a href=http://doi.org/c8jt >http://doi.org/c8jt</a></p>

    <hr>

    <p>This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    You should have received a copy of the GNU Affero General Public License
      along with this program. If not, see <a href=https://www.gnu.org/licenses/>https://www.gnu.org/licenses/</a>.</p>

    <hr>
    <p>Full code and license availabe at <a href=https://github.com/IrishMarineInstitute/nephrops-burrow-counting-app/>GitHub repo</a>.</p>
    * mikel.aristegui@marine.ie")), 
    
    # Setting for keyboard inputs
            tags$script('
            pressedKeyCount = 1;
            $(document).on("keydown", function (e) {
            Shiny.onInputChange("pressedKey", pressedKeyCount++);
            Shiny.onInputChange("pressedKeyId", e.which);
                        });')),
  
  
  # MIDDLE 1st ROW: display of images, ancillary data, and main output datatable
  fluidRow(
    # Display of images
    column(8,
           imageOutput("plot1"
                       ,click = clickOpts("plot_click",clip=T), # For annotation of image when clicking on it (circle)
                       # dblclick = "plot_dblclick",            # Other uses?
                       # hover = "plot_hover",                  # Other uses?
                       # brush = brushOpts("plot_brush",        # Alternative code: For annotation of image when brushing (square)
                       #                   stroke="#e00",
                       #                   clip=T),
                       height=700
                       )),
    
    
      # Ancillary data inputs and save button / matching plot
      column(4,
             # If not using SIC-matching method:
             conditionalPanel(condition="input.inreviewer.indexOf('SIC_matching') == -1",
             # Only for reviewers with ancillary
             conditionalPanel(condition="input.inreviewer.indexOf('1st reviewer') !== -1 ||
                              input.inreviewer.indexOf('only ancillary') !== -1",
             fluidRow(
               #hr(),
               column(2, uiOutput("vm")),
               column(2, uiOutput("fq")),
               column(2, uiOutput("pp")),
               column(2, uiOutput("kp")),
               column(2, uiOutput("trawl")),
               column(2, uiOutput("lt")))),
             
             fluidRow(
               # Only for reviewers with ancyllary:
               conditionalPanel(condition="input.inreviewer.indexOf('1st reviewer') !== -1 ||
                                input.inreviewer.indexOf('only ancillary') !== -1", 
               column(2, uiOutput("lm")),
                # Marine Institute version
               conditionalPanel(condition="input.inSurveyID.indexOf('IFREMER') == -1",
                                column(2, uiOutput("sql"))
               )
               ,
                # Ifremer version. Special request for counting Squat lobsters
               conditionalPanel(condition="input.inSurveyID.indexOf('IFREMER') !== -1",
                                column(2, uiOutput("sqlCN"), actionGroupButtons(c("sqlCless","sqlCmore"), c("-","+"), direction="horizontal", size="sm"))
               )
               ,
               column(2, uiOutput("fs"))),
               column(6, uiOutput("comm"))),
             
             # Only for reviewers with ancyllary:
             fluidRow(style = "height:35px;",
                      conditionalPanel(condition="input.inreviewer.indexOf('1st reviewer') !== -1 ||
                                       input.inreviewer.indexOf('only ancillary') !== -1", 
               column(3, uiOutput("nepInN"), actionGroupButtons(c("nepInless","nepInmore"), c("-","+"), direction="horizontal", size="sm")),
               column(3, uiOutput("nepOutN"), actionGroupButtons(c("nepOutless","nepOutmore"), c("-","+"), direction="horizontal", size="sm"))),
               #br(),
               
               # save button to write ancillary data to .csv file
               column(2,
                      actionButton("anciSave", "Save ancillary")),
               column(3, textOutput("anc.info"), offset=1))),
             
             # If using SIC-matching method:
             conditionalPanel(condition = "input.inreviewer.indexOf('SIC_matching') !== -1",
                     column(12, imageOutput("match_plot", height=340))),
             
             hr(),
             
             # Output datatable for the user. To check the burrow systems annotated
              # setting of the datatable
             tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                             color: #ffffff !important;
                             }
                             thead {
                             color: #ffffff;
                             }")),
              # datatable
             DTOutput("coordinates"),
              # Button to delete rows from datatable
             column(5, actionButton("delete", "Delete all burrows in current image"))
              # Button to save counts into database
             # ,column(4, actionButton("database", "UPLOAD burrow counts to Database"), offset=1)
             )),
  
  
  
  
  # MIDDLE 2nd ROW: Reset button, non-countable time input, current time info, speed selector
  #hr(),
fluidRow(
  
  # Reset button to delete all the annotated images and created .csv files for current Survey & Counter & Station
  column(1,
         br(),
         actionButton("reset", "Reset my counts"),
         tags$head(tags$style("#confirmReset{color: red;
                              font-size: 40px;
                              font-style: bold;
                              border-color: blue;
                              }")),
         # Reset button needs confirmation by the user to work
         bsModal("confirmReset", "WARNING!!", "reset", size = "large",
                 HTML("You are just about to delete all your counts.<br>
                      Any .csv file and annotated image you've created will be deleted.<br>
                      Are you sure you want to do this?"),
                 actionButton("confirmYes", "Yes"),
                 actionButton("confirmNo", "No"))
         ),
  
  # Speed selector
  column(1, selectInput("speed", "Speed",
                        choices = c("normal", "slow", "very slow", "super slow"), selected="normal")),
  column(1, tags$h6("(in lower speeds you'll need to pause the video before swaping between play/rewind)")),# speed issue info for user
  

  
  # Current time info
  column(1,
         textOutput("timer"),
         # setting timer format
         tags$head(tags$style("#timer{color: green;
                       font-size: 40px;
                       }"))),
  
  # Input for non-countable time (i.e. footage with sand clouds)
  
  conditionalPanel(condition="input.inreviewer.indexOf('1st reviewer') !== -1", column(2,verbatimTextOutput("textTime"),
                          actionGroupButtons(c("startTime","stopTime", "confirmTime"),
                                             c("start", "stop", "confirm"), # "confirm" button saves "start" and "stop" times into a .csv file
                                             direction="horizontal", size="sm"))),
  
  column(3, div(style = 'overflow-x: scroll', DTOutput("non_seconds_table"))),

  # Output datatable for the user. To check the non-countable time
  # setting of the datatable
  # tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
  #                            color: #ffffff !important;
  #                            }")),
  # datatable
  conditionalPanel(condition="input.inreviewer.indexOf('1st reviewer') !== -1",
  column(3, DTOutput("non_time_table"),
         # Button to delete rows from datatable
         actionButton("delete_time", "Delete row", style='padding:4px; font-size:80%')))),


  



# BOTTOM ROW: Slider

fluidRow(
  column(12,
         # display dynamic UI with a slider input to select image number
         uiOutput("slider")))

) # END OF ui




# Define server logic required to draw a histogram
server <- function(input, output, session) {

  volumes <- c(Home = "C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/reduced_stn", getVolumes()())
  volumes_parent <- c(Home = "C:/GitHub/Image-annotation-R-Shiny-app/Parent_folder/", getVolumes()())
  
  
  
  # Reading the surveys names and the counters names from the .csv file, which
  # must be filled by SIC before the surveys
  
  surveys_counters <- read.csv(paste0(volumes_parent[1], "/app_outcome/counts/SURVEYS_and_COUNTERS.csv"))
  
  surveys <- as.character(unique(surveys_counters$survey))
  
  # Dynamic dropdown for surveyID
  output$surveyID <- renderUI({
    selectInput("inSurveyID", "1st: current survey",
                c("select survey", surveys)
    )
  })
  
  surv <- reactive({input$inSurveyID})
  
  
  
  # Dynamic dropdown for counterID
  
  counters <- list()
  
  for (i in surveys) {
    cntrs <- as.character(surveys_counters[surveys_counters$survey == i,"counter"])
    counters[[i]] <- cntrs
  }


  output$counterID <- renderUI({
    if (input$inSurveyID == "select survey") {
      selectInput("inCounterID", "2nd: select your ID",
                  c("input survey"))
    } else if (is.null(input$pair.run)) {
      selectInput("inCounterID", "2nd: select your ID",
                  c("select ID",
                    counters[[surv()]], "SIC_matching"))
    } else if (input$pair.run == 0) {
      selectInput("inCounterID", "2nd: select your ID",
                  c("select pair"))
    } else if (input$pair.run == 1) {
      selectInput("inCounterID", "2nd: select your ID",
                  c(paste("SIC", unique(rv$tablebase$counter_ID)[1], unique(rv$tablebase$counter_ID)[2], sep = "_")))
    }
  })
  
  
  
  
  # Dynamic dropdown for stationID

    # List of folders with stations
  stnfoldersDirs <- dir(volumes[1], full.names=F)
  stnfoldersDirs <- grep(stnfoldersDirs, pattern=paste(".csv|.txt|.R|CredDB.CEF|.pdf"), inv=T, value=T)
  
  stations.numb <- stnfoldersDirs
  
  stations <- list()
  
    # TO ALLOW ALL THE COUNTERS TO READ ALL THE STATIONS
  for (i in surveys) {
    stations[[i]] <- sapply(counters[[i]], function(x) NULL)
    for(j in 1:length(counters[[i]])) {
      stations[[i]][[j]] <- stations.numb
    }
  }
  
    # MANUAL ASSIGNMENT OF STATIONS TO EACH COUNTER CAN BE SET UP HERE:
    # # assigning stations for leg 1
    # 
    # stations[["CV19015"]] <- sapply(counters[["CV19015"]], function(x) NULL)
    # for(i in 1:length(counters[["CV19015"]])) {
    #   stations[["CV19015"]][[i]] <- stations.numb
    # }
    # 
    # # assigning stations for leg 2
    # 
    # stations[["CV19017"]] <- sapply(counters[["CV19017"]], function(x) NULL)
    # for(i in 1:length(counters[["CV19017"]])) {
    #   stations[["CV19017"]][[i]] <- stations.numb
    # }
    # 
    # # assigning stations for leg 3
    # 
    # stations[["CV19022"]] <- sapply(counters[["CV19022"]], function(x) NULL)
    # 
    # for(i in 1:length(counters[["CV19022"]])) {
    #   stations[["CV19022"]][[i]] <- stations.numb
    # }
  
  
  output$stationID <- renderUI({
    if (grepl("SIC", input$inCounterID) == F) {
      selectInput("inStationID", "3rd: select station",
                  c("select stn",
                    stations[[surv()]][[input$inCounterID]])
      )
    } else {
      
      selectInput("inStationID", "3rd: select station",
                  as.character(rvp$pairs$stn[rvp$selectedRowPair]),
                               selected = as.character(rvp$pairs$stn[rvp$selectedRowPair])
      )
      }
    })
  
  output$reviewer <- renderUI({
    if (grepl("SIC", input$inCounterID) == F) {
      selectInput("inreviewer", "4th: 1st or 2nd?", c("reviewer n.", c("1st reviewer", "2nd reviewer", "only ancillary")))
    } else {
      selectInput("inreviewer", "4th: 1st or 2nd?", c("SIC_matching"))
    }
  })


    # Reading the Video operator ID from SURVEYS_and_COUNTERS.csv
  VidOpID <- reactive({
    # If not using SIC-matching method:
    if (input$inreviewer != "SIC_matching") {
      unique(surveys_counters[surveys_counters["counter"]==input$inCounterID,]$VideoOperatorID) # unique() needed in case the same counter is in more than one survey
    } else {
    # If using SIC-matching method:
      "999"
    }
    
    
  })
 

  
    # Selected station directory
  stationDir <- reactive({
    paste0(as.character(volumes[1]),"/", input$inStationID)
    })
  
  output$directorypath <- renderPrint({
    stationDir()
  })
  
    # Folder with images for the station
  foldersDirs <- reactive({
    paste0(stationDir(), "/reduced_images")
  })
  

  
  # Create "go" reactivevalue for. We use "go" to manage keyboard inputs below
  go <- reactiveVal(value=0)
  
  # start button activates "go", which activates keyboard inputs below
  observeEvent({input$start}, {
      go(input$start)
    }
  )

  # if the user changes the CounterID or StationID, "go" is inactivated
  observeEvent({
    input$inCounterID
    input$inStationID}, {
               go(0)
      }
    )
  
  
  # Creation of:
      # One text file with the original names of the images. This text file will be created the first time a station is loaded, and it will never be edited
      # One text file with the original names of the images for each counterID who loads the stationID. These text files will be edited as the counter annotates images

    # base reactivevalue for the text files
  jpgsFiles <- reactiveValues(jpgsNames = character(0),
                              originalNames = character(0))
  
    # when start button is pressed, create the text file with the original names of the images
    # Create .txt and counts.csvif the counter is SIC_counter1_counter2
  observeEvent({
    input$start
  }, {
    
    if (input$inSurveyID != "select survey" &input$inCounterID != "select ID" & input$inStationID != "select stn" & input$inreviewer != "reviewer n."){ # if all surveyID, counterID, stationID and reviewer have valid values
      
      # First we disable the selectinputbuttons
      shinyjs::disable("surveyID")
      shinyjs::disable("counterID")
      shinyjs::disable("stationID")
      shinyjs::disable("reviewer")
      shinyjs::disable("start")
      
          if(file_test("-f", 
               paste0(as.character(volumes_parent[1]),
                      "/reduced_stn/",
                      input$inStationID,
                      "/",
                      input$inSurveyID,"_",input$inStationID,
                      "_", "original",
                      "_images_list.txt")) == F) {
      
      # If this is the first time the station is selected ever,
      # then create a .txt file with the list of the original image names
            
      # read image names into "prenames"
      prenames <- list.files(foldersDirs(), pattern=paste0(".jpg"), full.names=T)
      
      # fill the base reactivevalue
      jpgsFiles$originalNames <- prenames
      jpgsFiles$jpgsNames <- prenames
      
      # create the .txt
      write.table(jpgsFiles$jpgsNames, paste0(as.character(volumes_parent[1]),
                                            "/reduced_stn/",
                                            input$inStationID,
                                            "/",
                                            input$inSurveyID,"_",input$inStationID,
                                            "_", "original",
                                            "_images_list.txt"),
                  row.names=F, col.names=F)
    } else {
      # If this is not the first time the station is selected
      
      readnames <- read.table(paste0(as.character(volumes_parent[1]),
                                     "/reduced_stn/",
                                     input$inStationID,
                                     "/",
                                     input$inSurveyID,"_",input$inStationID,
                                     "_", "original",
                                     "_images_list.txt"))
      
      jpgsFiles$originalNames <- as.character(readnames[,1])
      
      if(file_test("-f",
                   paste0(as.character(volumes_parent[1]),
                          "/reduced_stn/",
                          input$inStationID,
                          "/",
                          input$inSurveyID,"_",input$inStationID,
                          "_", input$inCounterID,
                          "_images_list.txt")) == F) {
        
      
        # If this CounterID has never counted this station before,
        # then read the .txt with the original names
        
        
        jpgsFiles$jpgsNames <- as.character(readnames[,1])
        
        if (input$inreviewer == "SIC_matching") { # Create .txt and counts.csvif the counter is SIC_counter1_counter2
          
          # Number of times we'll go through the loop
          withProgress(message = paste0('Creating ', nrow(rv$tablebase), ' annotated images:'), value = 0, {

          # Create images with both annotations for counter SIC_counter1_counter2
          for (i in 1:nrow(rv$tablebase)) {
            # print(jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/nrow(rv$tablebase), detail = paste0("annotation ", i, " of ", nrow(rv$tablebase)))


            # read the original .jpg picture
            im <- image_read(jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])

            # if it is the first burrow in the current still...
            if (!grepl(paste0("_burrow_", input$inCounterID, ".jpg"), jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])) {

              # do nothing (previous version of the app did something)
            } else { # if it is NOT the first burrow in the current still...

              file.remove(jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])
            }

            # copy im to im4, and draw a circumference on it where the user clicked in the picture
            im4 <- image_draw(im)

            symbols(as.numeric(rv$tablebase$x[i]), as.numeric(rv$tablebase$y[i]),
                    circles = 50,
                    fg = c("green3", "orangered", "steelblue", "orange")[as.numeric(as.factor(rv$tablebase$feature)[i])], inches = FALSE, add = TRUE)
            dev.off()

            # if it is the first burrow in the current still...
            if (!grepl(paste0("_burrow_", input$inCounterID, ".jpg"), jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])) {

              burrow.name <- gsub(".jpg", paste0("_1_burrow_", input$inCounterID, ".jpg"), jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])

              # new name of the image into the .txt file of the Counter
              jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])] <- burrow.name

              # create new image with new name
              image_write(im4, path = burrow.name, format = "jpg")

            } else { # if it is NOT the first burrow in the current still...

              burrow.name.old <- jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])]

              burrow.name.new <-  gsub(paste0("0_[^<]+burrow_", input$inCounterID, ".jpg"),
                                       paste0("0_",
                                              sum(rv$tablebase$still_n==as.numeric(rv$tablebase$still_n[i]))+1,
                                              "_burrow_", input$inCounterID, ".jpg"),
                                       jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])])

              # new name of the image into the .txt file of the Counter
              jpgsFiles$jpgsNames[as.numeric(rv$tablebase$still_n[i])] <- burrow.name.new
              # create new image with new name
              image_write(im4, path = burrow.name.new, format = "jpg")

            }
          }
            })
          
          # Write .txt with the jpg names for SIC_counter1_counter2
          write.table(jpgsFiles$jpgsNames, paste0(as.character(volumes_parent[1]),
                                                  "/reduced_stn/",
                                                  input$inStationID,
                                                  "/",
                                                  input$inSurveyID,"_",input$inStationID,
                                                  "_", input$inCounterID,
                                                  "_images_list.txt"),
                      row.names=F, col.names=F)

          # Write counts.csv for SIC_counter1_counter2
          write.table(rv$tablebase,
                      file = paste0(as.character(volumes_parent[1]), "/app_outcome/counts/",
                                    input$inSurveyID,"_",input$inStationID,
                                    "_", input$inCounterID,
                                    "_counts.csv"),
                      row.names = F,
                      sep = ",")
        }
        
        # We finish here the if (input$inreviewer == "SIC_matching")
        
      } else {
        # If this CounterID already has counted the station before,
        # then read the list created last time with the image names for this CounterID
        
        readnames <- read.table(paste0(as.character(volumes_parent[1]),
                                                 "/reduced_stn/",
                                                 input$inStationID,
                                                 "/",
                                                 input$inSurveyID,"_",input$inStationID,
                                                 "_", input$inCounterID,
                                                 "_images_list.txt"))
        
        jpgsFiles$jpgsNames <- as.character(readnames[,1])
        
        }
      }
      
    } else {
      showModal(modalDialog(title ="Please, select your:",
                            HTML("Survey,<br>
                            ID,<br>
                            station number and<br>
                            reviewer number<br>
                            before loading the station")))
    }
    
  })
  
  # check how many .jpgs are for the station

  output$jpgnumber <- renderPrint({
    length(jpgsFiles$jpgsNames)
  })
  

  # speed input for setting the sleeping time between the display of each image
  sleep <- reactive({
    if (input$speed == "normal") {
      s <- 0
    }
    if (input$speed == "slow") {
      s <- 0.1
    }
    if (input$speed == "very slow") {
      s <- 0.2
    }
    if (input$speed == "super slow") {
      s <- 0.3
    }
    return(s)
  })

  

  # aa is the still that we want to display (it will be a number)
  aa <- reactive({

    if (go() > 0) { # only works if go is activated

    if (is.null(input$coordinates_rows_selected)) { # only works if there is no row selected on the table


    if (input$pressedKeyId == 39) { # right key for play
      a <- input$inSlider + 1
    }


    if (input$pressedKeyId == 37) { # left key for backwards
      a <- input$inSlider - 1
    }


    if (input$pressedKeyId == 38) { #  up key for pause
      a <- isolate(input$inSlider)
    }

    if (input$pressedKeyId == 40) { # down key for pause
      a <- isolate(input$inSlider)
    }
    }

    if (!is.null(input$coordinates_rows_selected)) { # if there is a row selected in the table

      a <- last$old # value coming from the selected row in the table

    }
      }

    Sys.sleep(sleep()) # if the speed selected is not "normal" there will be a delay between images

    return(a)

  })

 
  # For staying in the same still after deleting the burrows from this still
  last <- reactiveValues(old=numeric(0), new=numeric(0))
  
  observeEvent(input$coordinates_rows_selected, {
    if (length(last$new)==0) {
      last$old <- rv$tablebase[input$coordinates_rows_selected,]$still_n
    }
  })
  

  
  # Make dynamic slider
  output$slider <- renderUI({

    sliderInput("inSlider", ".jpg number", min=1, max=length(jpgsFiles$jpgsNames), value=aa(), step=1, width="100%"
                #,animate=animationOptions(interval = 150, loop = FALSE, playButton = NULL, pauseButton = NULL)
                )
    
  })
  
  
  # Reading the Time from the exif info of the images when we load the station
  all.times <- eventReactive(input$start, {
    
    options(digits.secs=6)
    
    ex <- read_exif(jpgsFiles$jpgsNames[c(1, length(jpgsFiles$jpgsNames))], tags=c("SourceFile", "DateTimeOriginal", "GPSLongitude", "GPSLatitude", "GPSTimeStamp"))
    
    tot.time <- difftime(as.POSIXct(ex$DateTimeOriginal[2], format="%Y-%b-%d %H:%M:%OS"),
                         as.POSIXct(ex$DateTimeOriginal[1], format="%Y-%b-%d %H:%M:%OS"),
                         units = "secs")
    
    return(hms(seconds = seq(0, as.numeric(tot.time), length=length(jpgsFiles$jpgsNames))))
    
  })
  
  # displaying the time of the current image as mm:ss.
  # Time as 00:00 first image, XX:XX last image (usually 10:00 for UWTV footage)
  output$timer <- renderText({
    substring(all.times()[input$inSlider], 4, 8)
  })
  
  
  # Creating the three output tables
  
  ## Table for annotations of burrows
  n_tablebase <- 11
  rv <- reactiveValues(tablebase = setNames(data.frame(matrix(ncol = n_tablebase, nrow = 0)), c("survey",
                                                                                                "station",
                                                                                                "counter_ID",
                                                                                                "time",
                                                                                                "still_n",
                                                                                                "feature",
                                                                                                "x","y",
                                                                                                "annotation_time",
                                                                                                "VideoOperatorID",
                                                                                                "minute")))
  # rv <- reactiveValues(tablebase = data.frame("survey" = character(),
  #                                             "station" = character(),
  #                                             "counter_ID" = character(),
  #                                             "time" = character(),
  #                                             "still_n" = integer(),
  #                                             "feature" = character(),
  #                                             "x" = character(), "y" = character(),
  #                                             "annotation_time" = character(),
  #                                             "VideoOperatorID" = character(),
  #                                             "minute" = character()))
  ## Table for non-countable time  
  rvTime <- reactiveValues(tableTime = setNames(data.frame(matrix(ncol = 9, nrow = 0)), c("survey",
                                                                                          "station",
                                                                                          "counter_ID",
                                                                                          "start_non_countable",
                                                                                          "stop_non_countable",
                                                                                          "feature",
                                                                                          "VideoOperatorID",
                                                                                          "minute",
                                                                                          "seconds")))

  
  rvSeconds <- reactiveValues(tableNonsecs = setNames(data.frame(matrix(ncol = 6, nrow = 20)), c("survey", # 20 by default
                                                                                              "station",
                                                                                              "counter_ID",
                                                                                              "VideoOperatorID",
                                                                                              "minute",
                                                                                              "seconds_off")))
  observeEvent({input$start}, { # but, as soon as we press input$start, we take the real number of minutes in the footage
    rvSeconds$tableNonsecs <- rvSeconds$tableNonsecs[1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1),]
  })
  
  
  ## Table for ancillary data  
  n_variables <- 16
  rvAncillary <- reactiveValues(tableAncillary = setNames(data.frame(matrix(ncol = n_variables, nrow = 0)), c("survey",
                                                                                                              "station",
                                                                                                              "counter_ID",
                                                                                                              "VAM", "FAQ", "PNP", "KOP",
                                                                                                              "LMC", "squat_lobster", "fish",
                                                                                                              "Nephrops_IN",
                                                                                                              "Nephrops_OUT",
                                                                                                              "trawl_marks",
                                                                                                              "litter",
                                                                                                              "Comments",
                                                                                                              "VideoOperatorID"
                                                                                                              )))

  
  # If the .csv files for these tables are already created, then read them when clicking step 4:
  observeEvent({input$start}, {
    # annotations
    if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/counts/",
                        input$inSurveyID,"_",input$inStationID,
                        "_", input$inCounterID,
                        "_counts.csv")) == T) {
      rv$tablebase <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/counts/",
                                      input$inSurveyID,"_",input$inStationID,
                                      "_", input$inCounterID,
                                      "_counts.csv"),
                               # colClasses = rep("character", n_tablebase),
                               stringsAsFactors = FALSE)
    }
    # ancillary
    if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/ancillary/",
                        input$inSurveyID,
                        "_", input$inStationID,
                        "_", input$inCounterID,
                        "_ancillary.csv")) == T) {
      rvAncillary$tableAncillary <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/ancillary/",
                                          input$inSurveyID,
                                          "_", input$inStationID,
                                          "_", input$inCounterID,
                                          "_ancillary.csv"),
                                   colClasses = rep("character", n_variables))
      
      anc.time$modif <- as.character(file.info(paste0(as.character(volumes_parent[1]),"/app_outcome/ancillary/",
                                                     input$inSurveyID,
                                                     "_", input$inStationID,
                                                     "_", input$inCounterID,
                                                     "_ancillary.csv"))$mtime)
      
     } 
    # else {
    #     rvAncillary$tableAncillary <- data.frame("survey"=NA,
    #                                              "station"=NA,
    #                                              "counter_ID"=NA,
    #                                              "VAM"=NA, "FAQ"=NA, "PNP"=NA, "KOP"=NA,
    #                                              "LMC"=NA, "squat_lobster"=NA, "fish"=NA,
    #                                              "Nephrops_IN"=0,
    #                                              "Nephrops_OUT"=0,
    #                                              "trawl_marks"=NA,
    #                                              "litter"=NA,
    #                                              "Comments"= as.character(" "),
    #                                              "VideoOperatorID"=NA)
    #     }
    
    # non-countable time
    if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/non_countable_time/",
                        input$inSurveyID,
                        "_", input$inStationID,
                        "_", input$inCounterID,
                        "_non_countable_time.csv")) == T) {
      rvTime$tableTime <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/",
                                          input$inSurveyID,
                                          "_", input$inStationID,
                                          "_", input$inCounterID,
                                          "_non_countable_time.csv"),
                                   colClasses = rep("character", 9))
    }

    # seconds_off time
    if(length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                  pattern = paste0(input$inSurveyID,
                  "_", input$inStationID,
                  "_", ".*.",
                  "_seconds_off.csv"))) > 1) { # If there is more than one
                    
                    showModal(modalDialog(title ="Error: Contact SIC.",
                                          HTML("There are more than one .csv file with non_countable_time data for this station.<br>
                                          SIC must check the app_outcome/non_countable_time folder and delete the duplicated .csv files that shouldn't be there.")))
                    
                  } else if (length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                                        pattern = paste0(input$inSurveyID,
                                        "_", input$inStationID,
                                        "_", ".*.",
                                        "_seconds_off.csv"))) == 1) { # if there is one

                          rvSeconds$tableNonsecs <- read.csv(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                                             pattern = paste0(input$inSurveyID,
                                             "_", input$inStationID,
                                             "_", ".*.",
                                             "_seconds_off.csv")),
                                         colClasses = rep("character", 6))
                          
                  } else if (length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                                        pattern = paste0(input$inSurveyID,
                                        "_", input$inStationID,
                                        "_", ".*.",
                                        "_seconds_off.csv"))) == 0) { # if there is none

                    rvSeconds$tableNonsecs$seconds_off[1] <- "no_seconds_have_been_removed_from_this_station_yet"
                    
                  }

  })
  
  # ANCILLARY data table
  
  output$vm <- renderUI({
    selectInput("invm", "VAM", choices = list("no","yes"), selected=rvAncillary$tableAncillary$VAM)
  })
  output$fq <- renderUI({
    selectInput("infq", "FAQ", choices = list("no","yes"), selected=rvAncillary$tableAncillary$FAQ)
  })
  output$pp <- renderUI({
    selectInput("inpp", "PNP", choices = list("no","yes"), selected=rvAncillary$tableAncillary$PNP)
  })
  output$kp <- renderUI({
    selectInput("inkp", "KOP", choices = list("no","yes"), selected=rvAncillary$tableAncillary$KOP)
  })
  output$lm <- renderUI({
    selectInput("inlm", "LMC", choices = list("no","yes"), selected=rvAncillary$tableAncillary$LMC)
  })
  output$sql <- renderUI({
    selectInput("insql", "Squat", choices = list("no","yes"), selected=rvAncillary$tableAncillary$squat_lobster)
  })
  # output$sqlCN <- renderUI({
  #   textOutput(rvAncillary$tableAncillary$squat_lobster)
  # })
  
  output$fs <- renderUI({
    selectInput("infs", "Fish", choices = list("no","yes"), selected=rvAncillary$tableAncillary$fish)
  })
  output$comm <- renderUI({
    textAreaInput("incomm", "Comments", value=rvAncillary$tableAncillary$Comments, height = "35px", placeholder="Add comments here")
  })
  
  # output$nepInN <- renderUI({
  #   textOutput(rvAncillary$tableAncillary$Nephrops_IN)
  # })
  # output$nepOutN <- renderUI({
  #   textOutput(rvAncillary$tableAncillary$Nephrops_OUT)
  # })
  
  output$trawl <- renderUI({
    selectInput("intrawl", "Trawl marks", choices = list("no","yes"), selected=rvAncillary$tableAncillary$trawl_marks)
  })
  output$lt <- renderUI({
    selectInput("inlt", "Litter", choices = list("no","yes"), selected=rvAncillary$tableAncillary$litter)
  })
  
  
  # reactive object that updates everytime any ancillary input is changed by the user
  all.update <- reactive(paste0(input$invm, input$infq, input$inpp, input$inkp,
                                input$inlm,
                                input$insql, input$sqlCmore, input$sqlCless,
                                input$infs,
                                input$nepInmore, input$nepInless,
                                input$nepOutmore, input$nepOutless,
                                input$intrawl,
                                input$inlt))
  com.update <- debounce(reactive(paste0(input$incomm)), millis = 10000) # wait 10 seconds to give time to the user to finish writing the comment
  
  anc.update <- reactive(paste0(all.update(), com.update()))
  
 # saving original nephrops counts from previously saved ancillary data
  orig.nepIn <- eventReactive({input$start}, {
    
    if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/ancillary/",
                        input$inSurveyID,
                        "_", input$inStationID,
                        "_", input$inCounterID,
                        "_ancillary.csv")) == T) {
      return(as.numeric(rvAncillary$tableAncillary$Nephrops_IN))
      
      } else { if (input$inreviewer %in% c("1st reviewer", "only ancillary")){
        return(0) # if there is no data, then 0
        } else if (input$inreviewer == "2nd reviewer"){
          return(-1) # -1 to know that this is the 2nd reviewer
        }}
    })
  
  orig.nepOut <- eventReactive({input$start}, {
    
    if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/ancillary/",
                        input$inSurveyID,
                        "_", input$inStationID,
                        "_", input$inCounterID,
                        "_ancillary.csv")) == T) {
      return(as.numeric(rvAncillary$tableAncillary$Nephrops_OUT))
      
    } else { if (input$inreviewer %in% c("1st reviewer", "only ancillary")){
      return(0) # if there is no data, then 0
    } else if (input$inreviewer == "2nd reviewer"){
      return(-1) # -1 to know that this is the 2nd reviewer
    }}
  })
  
  # For Ifremer version
  orig.sqlC <- eventReactive({input$start}, {
    
    if(grepl("IFREMER", input$inSurveyID)) {
      
          if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/ancillary/",
                        input$inSurveyID,
                        "_", input$inStationID,
                        "_", input$inCounterID,
                        "_ancillary.csv")) == T) {
      return(as.numeric(rvAncillary$tableAncillary$squat_lobster))
      
    } else { if (input$inreviewer %in% c("1st reviewer", "only ancillary")){
      return(0) # if there is no data, then 0
    } else if (input$inreviewer == "2nd reviewer"){
      return(-1) # -1 to know that this is the 2nd reviewer
      
    }}
      
      }
    
  })
  
  # Edit for Ifremer version.
  sqlCfinal <- reactive({
    if (is.null(input$inSurveyID)) {
      sqlCfin <- input$insql
      } else if (grepl("IFREMER", input$inSurveyID) == F) { # MI version. Squat presence/absence
        sqlCfin <- input$insql
        } else if (grepl("IFREMER", input$inSurveyID) == T) { # IFREMER version. Squat number
          sqlCfin <- (orig.sqlC() + input$sqlCmore - input$sqlCless)
          }
    return(sqlCfin)
  })
  
  # changing any ancillary input will create a .csv file with the ancillary data
  observeEvent({anc.update()}, {

      if (paste0(input$invm, input$infq, input$inpp, input$inkp,
                 input$inlm,
                 input$insql, input$sqlCmore, input$sqlCless,
                 input$infs,
                 input$nepInmore, input$nepInless,
                 input$nepOutmore, input$nepOutless,
                 input$incomm,
                 input$intrawl,
                 input$inlt) != "nononononono00no0000nono") { # to avoid saving without inputs from the scientist
 
      # if (input$start > 0) {
  
      # rvAncillary$tableAncillary$Comments <- as.character(rvAncillary$tableAncillary$Comments)
        
      rvAncillary$tableAncillary[1,] = c(input$inSurveyID,
                                         input$inStationID,
                                         input$inCounterID,
                                         input$invm, input$infq, input$inpp, input$inkp,
                                         input$inlm,
                                         sqlCfinal(),
                                         input$infs,
                                         (orig.nepIn() + input$nepInmore - input$nepInless),
                                         (orig.nepOut() + input$nepOutmore - input$nepOutless),
                                         input$intrawl,
                                         input$inlt,
                                         input$incomm,
                                         VidOpID())
  
      write.table(rvAncillary$tableAncillary,
                  file = paste0(as.character(volumes_parent[1]), "/app_outcome/ancillary/",
                                input$inSurveyID,
                                "_",input$inStationID,
                                "_", input$inCounterID,
                                "_ancillary.csv"),
                  row.names = F,
                  sep = ",")
  
      anc.time$modif <- as.character(file.info(paste0(as.character(volumes_parent[1]),"/app_outcome/ancillary/",
                                                      input$inSurveyID,
                                                      "_", input$inStationID,
                                                      "_", input$inCounterID,
                                                      "_ancillary.csv"))$mtime)
      }
    # }
    })
  


  
  
  # clicking on the save button will create a .csv file with the ancillary data
  observeEvent({input$anciSave},{
    
      rvAncillary$tableAncillary[1,] = c(input$inSurveyID,
                                         input$inStationID,
                                         input$inCounterID,
                                         input$invm, input$infq, input$inpp, input$inkp,
                                         input$inlm,
                                         sqlCfinal(),
                                         input$infs,
                                         (orig.nepIn() + input$nepInmore - input$nepInless),
                                         (orig.nepOut() + input$nepOutmore - input$nepOutless),
                                         input$intrawl,
                                         input$inlt,
                                         input$incomm,
                                         VidOpID())

      write.table(rvAncillary$tableAncillary,
                  file = paste0(as.character(volumes_parent[1]), "/app_outcome/ancillary/",
                                input$inSurveyID,
                                "_",input$inStationID,
                                "_", input$inCounterID,
                                "_ancillary.csv"),
                  row.names = F,
                  sep = ",")
    
      anc.time$modif <- as.character(file.info(paste0(as.character(volumes_parent[1]),"/app_outcome/ancillary/",
                                                     input$inSurveyID,
                                                     "_", input$inStationID,
                                                     "_", input$inCounterID,
                                                     "_ancillary.csv"))$mtime)

      
  })
  
  # Every time the ancillary .csv file is changed, record time to show it for the user
  anc.time <- reactiveValues(modif = as.character())
  output$anc.info <- renderText(
    paste0("Last saved: ", substring(anc.time$modif, 1, 19))
  )
    
  # NON-COUNTABLE TIME
  textStartStop <- reactiveValues(start = integer(0),
                                  stop = integer(0))
  
  observeEvent({input$startTime},{
    textStartStop$start <- substring(all.times()[input$inSlider], 1, 8)
  })
  observeEvent({input$stopTime},{
    textStartStop$stop <- substring(all.times()[input$inSlider], 1, 8)
  })
  # showing to the operator the time section selected
  output$textTime <- renderText({
    paste0("log non-countable time:\n", substring(textStartStop$start, 4, 8 ), "-", substring(textStartStop$stop, 4, 8))
  })
  
  # clicking on the confirmTime button will create a .csv file with the new non-countable time section
  # and create seconds_off table and .csv file

  observeEvent({input$confirmTime},{
    
    # Only if there is no seconds_off.csv file created for this station
    if (length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                    pattern = paste0(input$inSurveyID,
                                     "_", input$inStationID,
                                     "_", ".*.",
                                     "_seconds_off.csv"))) == 0 | rvSeconds$tableNonsecs$counter_ID == input$inCounterID) {
      
        if (substring(textStartStop$start, 4, 5) != substring(textStartStop$stop, 4, 5)) {
      showModal(modalDialog(title ="Warning:",
                            HTML("Non-countable time section must be in one unique minute<br>
                                 Set 'start' and 'stop' times in the same minute")))
      
    } else if (substring(textStartStop$start, 7, 8) > substring(textStartStop$stop, 7, 8)) {
      showModal(modalDialog(title ="Warning:",
                            HTML("Start time must be set up before stop time")))
      } else {
      
      rvTime$tableTime[nrow(rvTime$tableTime)+1,] = c(input$inSurveyID,
                                                      input$inStationID,
                                                      input$inCounterID,
                                                      textStartStop$start,
                                                      textStartStop$stop,
                                                      "non_countable_time",
                                                      VidOpID(),
                                                      as.numeric(substring(textStartStop$start, 4, 5)) + 1,
                                                      paste(substring(textStartStop$start, 7,9):substring(textStartStop$stop, 7,9), collapse="_"))
      
      write.table(rvTime$tableTime,
                file = paste0(as.character(volumes_parent[1]), "/app_outcome/non_countable_time/",
                              input$inSurveyID,
                              "_",input$inStationID,
                              "_", input$inCounterID,
                              "_non_countable_time.csv"),
                row.names = F,
                sep = ",")
      
      non_calc <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/",
                                  input$inSurveyID,
                                  "_", input$inStationID,
                                  "_", input$inCounterID,
                                  "_non_countable_time.csv"),
                           colClasses = rep("character", 9))

      
      
      rvSeconds$tableNonsecs$survey <- input$inSurveyID
      rvSeconds$tableNonsecs$station <- input$inStationID
      rvSeconds$tableNonsecs$counter_ID <- input$inCounterID
      rvSeconds$tableNonsecs$VideoOperatorID <- VidOpID()
      rvSeconds$tableNonsecs$minute <- 1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)
      for(i in unique(rvSeconds$tableNonsecs$minute)) {
        non_calc_cur <- subset(non_calc, minute == i)
        non_secs_cur <- length(unique(as.numeric(unlist(strsplit(non_calc_cur$seconds, '_')))))
        rvSeconds$tableNonsecs[which(rvSeconds$tableNonsecs$minute == i),]$seconds_off <- non_secs_cur
      }
      write.table(rvSeconds$tableNonsecs,
                  file = paste0(as.character(volumes_parent[1]), "/app_outcome/non_countable_time/",
                                input$inSurveyID,
                                "_",input$inStationID,
                                "_", input$inCounterID,
                                "_seconds_off.csv"),
                  row.names = F,
                  sep = ",")
      
      # rvColor <- setNames(data.frame(matrix(ncol = (as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1), nrow = 3)), c(1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)))
      # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
      # rvColor <- rvColor[1:2,-c(21:40)]
      # rvColor[1,] <- df[1,]
      # rvColor[2,] <- df[2,]
      # dataCol_df <- ncol(rvColor)
      # dataColRng <- 1:dataCol_df
      # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
      # rvColor[2, argColRng] <- as.numeric(rvColor[2, dataColRng]) > 30
      
      # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
      # rvColor() <- rvColor()[1:2,-c(21:40)]
      # rvColor()[1,] <- df[1,]
      # rvColor()[2,] <- df[2,]
      # dataCol_df <- ncol(rvColor())
      # dataColRng <- 1:dataCol_df
      # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
      # rvColor()[2, argColRng] <- as.numeric(rvColor()[2, dataColRng]) > 30
      
      # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
      # rvColor$df <- rvColor$df[1:2,-c(21:40)]
      # rvColor$df[1,] <- df[1,]
      # rvColor$df[2,] <- df[2,]
      # dataCol_df <- ncol(rvColor$df)
      # dataColRng <- 1:dataCol_df
      # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
      # rvColor$df[2, argColRng] <- as.numeric(rvColor$df[2, dataColRng]) > 30
      
    }  
      
    } else {
      
      showModal(modalDialog(title ="Warning:",
                            HTML("You cannot log non-countable data for this station because another user has already logged this data. <br>
                                 Are you sure you are the 1st reviewer and that you have to log non-countable data?")))
    }
    

    
  })
  
  

  
  
  

  # DISPLAYING THE IMAGE SELECTED IN THE SLIDER
  output$plot1 <- renderImage({
    
    list(src=jpgsFiles$jpgsNames[input$inSlider],
         contentType = "image/png",
         alt = "waiting for station input"
         )
    
  }, deleteFile = F)
  



  # basic table with the info of the burrows as we click the images
  
    feat <- reactiveValues(what = NULL, # reactive value to be stored in the table (right now the only option is "BURROW"; more could be implemented with more actionbuttons)
                           counter = integer(0), # reactive value that will trigger the observeEvent for creating a new row in the table
                           original.vector = character(0), # vector to store all the original .jpg names as they were in raw
                           burrow.vector = character(0), # vector to store all the .jpg names with burrows
                           no.vector = character(0)) # vector to store all the .jpg names with "_no_counterID.jpg". These .jpgs are copies of the raw pictures, just with a different name
    
    # annotating the burrows
observeEvent(input$plot_click, {
  
  if(input$pressedKeyId == 38) { # only works if the video is paused with the up arrow key
  
    feat$what <- "BURROW"
    
    # everytime input$plot_click is clicked, the change of feat$counter will trigger the observeEvent for creating a new row in the table
    feat$counter <- paste0(feat$what, input$plot_click, input$inSlider) 
    
    # read the original .jpg picture
    im <- image_read(jpgsFiles$jpgsNames[input$inSlider])
    
    # if it is the first burrow in the current still...
    if (!grepl(paste0("_burrow_", input$inCounterID, ".jpg"), jpgsFiles$jpgsNames[input$inSlider])) {
      
      # do nothing (previous version of the app did something)

    } else { # if it is NOT the first burrow in the current still...
      
      file.remove(jpgsFiles$jpgsNames[input$inSlider])
    }
        
    # copy im to im4, and draw a circumference on it where the user clicked in the picture
    im4 <- image_draw(im)
    
    symbols(as.numeric(input$plot_click[1]), as.numeric(input$plot_click[2]),
            circles = 50,
            fg = "blue", inches = FALSE, add = TRUE)
    dev.off()
    
    
    # if it is the first burrow in the current still...
    if (!grepl(paste0("_burrow_", input$inCounterID, ".jpg"), jpgsFiles$jpgsNames[input$inSlider])) {
      
    burrow.name <- gsub(".jpg", paste0("_1_burrow_", input$inCounterID, ".jpg"), jpgsFiles$jpgsNames[input$inSlider])
    
    # new name of the image into the .txt file of the Counter
    jpgsFiles$jpgsNames[input$inSlider] <- burrow.name
    
    # create new image with new name
    image_write(im4, path = burrow.name, format = "jpg")
    
    } else { # if it is NOT the first burrow in the current still...
      
      burrow.name.old <- jpgsFiles$jpgsNames[input$inSlider]
      
      burrow.name.new <-  gsub(paste0("0_[^<]+burrow_", input$inCounterID, ".jpg"),
                              paste0("0_",
                                     sum(rv$tablebase$still_n==rv$tablebase$still_n[nrow(rv$tablebase)])+1,
                                     "_burrow_", input$inCounterID, ".jpg"),
                              jpgsFiles$jpgsNames[input$inSlider])
      
      # new name of the image into the .txt file of the Counter
      jpgsFiles$jpgsNames[input$inSlider] <- burrow.name.new
      # create new image with new name
      image_write(im4, path = burrow.name.new, format = "jpg")
      
        }
    
    
    
    
    
    
    
    
    
    
  } else { # if the video is not paused with the up arrow key
    showModal(modalDialog(title ="For correct annotation:",
                          HTML("(1) video must be in pause  (make sure pressing up arrow) and<br>
                                (2) wait until a blue circle appears in the picture")))
  }
  })



# Delete all the burrows in current image
observeEvent(input$delete, {
  
  # It only works if there is a row from the table selected 
  if(!is.null(input$coordinates_rows_selected)) {
    
    
    last$old <- rv$tablebase[input$coordinates_rows_selected,]$still_n
    #aa <- rv$tablebase$still_n[input$coordinates_rows_selected]
    
    # remove file with burrows
    file.remove(jpgsFiles$jpgsNames[input$inSlider])
    
    # set the original name of the .jpg to the list of .jpgs (swapping it with the name of the .jpg with burrows)
    jpgsFiles$jpgsNames[input$inSlider] <- jpgsFiles$originalNames[input$inSlider]

    # delete all the rows with the same still_number as the row that the user is deleting
    rv$tablebase <- rv$tablebase[rv$tablebase[,"still_n"] != rv$tablebase$still_n[input$coordinates_rows_selected],]
    
    # Write .txt with the jpg names for this CounterID
    write.table(jpgsFiles$jpgsNames, paste0(as.character(volumes_parent[1]),
                                            "/reduced_stn/",
                                            input$inStationID,
                                            "/",
                                            input$inSurveyID,"_",input$inStationID,
                                            "_", input$inCounterID,
                                            "_images_list.txt"),
                row.names=F, col.names=F)
    
    
    write.table(rv$tablebase,
                file = paste0(as.character(volumes_parent[1]), "/app_outcome/counts/",
                              input$inSurveyID,"_",input$inStationID,
                              "_", input$inCounterID,
                              "_counts.csv"),
                row.names = F,
                sep = ",")
    
    # For staying in the same still after deleting the burrows from this still
    last$new <- 1
    last$new <- numeric(0)
    
  } else {
    showModal(modalDialog(title ="No burrow selected",
                          HTML("To clear an image, please:<br>
                               (1) video must be in pause,<br>
                               (2) select a row in the table and<br>
                               (3) press 'Delete all burrows in current image' button")))
  }
  })

# Reseting all the counts needs confirmation
observeEvent(input$confirmNo, {
  toggleModal(session, "confirmReset", toggle = "close")
})
  # This removes .txt, counts.csv and annotated images of the selected Counter for the selected Station
observeEvent(input$confirmYes, {
  toggleModal(session, "confirmReset", toggle = "close")

  file.remove(grep("_burrow_", jpgsFiles$jpgsNames, value=T))
  
  jpgsFiles$jpgsNames <- jpgsFiles$originalNames
  
  rv$tablebase <- setNames(data.frame(matrix(ncol = n_tablebase, nrow = 0)), c("survey",
                                                                               "station",
                                                                               "counter_ID",
                                                                               "time",
                                                                               "still_n",
                                                                               "feature",
                                                                               "x", "y",
                                                                               "annotation_time",
                                                                               "VideoOperatorID",
                                                                               "minute"))
  
  # Remove the .txt and the .csv for this counter
  
  file.remove(paste0(as.character(volumes_parent[1]),
                     "/reduced_stn/",
                     input$inStationID,
                     "/",
                     input$inSurveyID,"_",input$inStationID,
                     "_", input$inCounterID,
                     "_images_list.txt"))
  
  file.remove(paste0(as.character(volumes_parent[1]),"/app_outcome/counts/",
                     input$inSurveyID,"_",input$inStationID,
                     "_", input$inCounterID,
                     "_counts.csv"))
  })

# Delete non_countable_time row
observeEvent(input$delete_time, {
  
  # It only works if there is a row from the table selected 
  if(!is.null(input$non_time_table_rows_selected)) {
    
    
    # delete selected row
    rvTime$tableTime <- rvTime$tableTime[-input$non_time_table_rows_selected,]
    
    # Write non_countable_time and seconds_off .csv files

    write.table(rvTime$tableTime,
                file = paste0(as.character(volumes_parent[1]), "/app_outcome/non_countable_time/",
                              input$inSurveyID,
                              "_",input$inStationID,
                              "_", input$inCounterID,
                              "_non_countable_time.csv"),
                row.names = F,
                sep = ",")
    
    non_calc <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/",
                                input$inSurveyID,
                                "_", input$inStationID,
                                "_", input$inCounterID,
                                "_non_countable_time.csv"),
                         colClasses = rep("character", 9))
    
    rvSeconds$tableNonsecs$survey <- input$inSurveyID
    rvSeconds$tableNonsecs$station <- input$inStationID
    rvSeconds$tableNonsecs$counter_ID <- input$inCounterID
    rvSeconds$tableNonsecs$VideoOperatorID <- VidOpID()
    rvSeconds$tableNonsecs$minute <- 1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)
    for(i in unique(rvSeconds$tableNonsecs$minute)) {
      non_calc_cur <- subset(non_calc, minute == i)
      non_secs_cur <- length(unique(as.numeric(unlist(strsplit(non_calc_cur$seconds, '_')))))
      rvSeconds$tableNonsecs[which(rvSeconds$tableNonsecs$minute == i),]$seconds_off <- non_secs_cur
    }
    write.table(rvSeconds$tableNonsecs,
                file = paste0(as.character(volumes_parent[1]), "/app_outcome/non_countable_time/",
                              input$inSurveyID,
                              "_",input$inStationID,
                              "_", input$inCounterID,
                              "_seconds_off.csv"),
                row.names = F,
                sep = ",")
    
    # rvColor <- setNames(data.frame(matrix(ncol = 20, nrow = 3)), c(1:20))
    # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
    # rvColor <- rvColor[1:2,-c(21:40)]
    # rvColor[1,] <- df[1,]
    # rvColor[2,] <- df[2,]
    # dataCol_df <- ncol(rvColor)
    # dataColRng <- 1:dataCol_df
    # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
    # rvColor[2, argColRng] <- as.numeric(rvColor[2, dataColRng]) > 30
    
    # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
    # rvColor() <- rvColor()[1:2,-c(21:40)]
    # rvColor()[1,] <- df[1,]
    # rvColor()[2,] <- df[2,]
    # dataCol_df <- ncol(rvColor())
    # dataColRng <- 1:dataCol_df
    # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
    # rvColor()[2, argColRng] <- as.numeric(rvColor()[2, dataColRng]) > 30
    
    # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
    # rvColor$df <- rvColor$df[1:2,-c(21:40)]
    # rvColor$df[1,] <- df[1,]
    # rvColor$df[2,] <- df[2,]
    # dataCol_df <- ncol(rvColor$df)
    # dataColRng <- 1:dataCol_df
    # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
    # rvColor$df[2, argColRng] <- as.numeric(rvColor$df[2, dataColRng]) > 30

    
  } else {
    showModal(modalDialog(title ="No row selected",
                          HTML("To delete a row, please:<br>
                               (1) selec a row in the table and<br>
                               (2) press 'Delete row' button")))
  }
})


# Ancillary data for Nephrops in and out

output$nepInN <- renderText({
  paste0("Nephrops IN: ", (orig.nepIn() + input$nepInmore - input$nepInless))
})

output$nepOutN <- renderText({
  paste0("Nephrops OUT: ", (orig.nepOut() + input$nepOutmore - input$nepOutless))
})

output$sqlCN <- renderText({
  paste0("Squat lobsters: ", (orig.sqlC() + input$sqlCmore - input$sqlCless))
})





# Table with all the info of the annotations, updated every time there is a new annotation
observeEvent({feat$counter}, {
  if (is.null(feat$what) | is.null(input$plot_click) | is.null(input$inSlider)) return()
  rv$tablebase[nrow(rv$tablebase)+1,] = c(input$inSurveyID,
                                          input$inStationID,
                                          input$inCounterID,
                                          substring(all.times()[input$inSlider], 4, 8),
                                          input$inSlider,
                                          feat$what,
                                          input$plot_click[1:2],
                                          format(Sys.time(), "%d-%b-%Y %X"),
                                          VidOpID(),
                                          as.numeric(substring(all.times()[input$inSlider], 4, 5)) + 1)
  # Order rv$tablebase by still number
  rv$tablebase <- rv$tablebase[order(as.numeric(as.character(rv$tablebase$still_n))),]
  
  # Write .txt with the jpg names for this CounterID (updated every new annotation)
  write.table(jpgsFiles$jpgsNames, paste0(as.character(volumes_parent[1]),
                                          "/reduced_stn/",
                                          input$inStationID,
                                          "/",
                                          input$inSurveyID,"_",input$inStationID,
                                          "_", input$inCounterID,
                                          "_images_list.txt"),
              row.names=F, col.names=F)
  
  # Write counts.csv for this CounterID (updated every new annotation)
  write.table(rv$tablebase,
              file = paste0(as.character(volumes_parent[1]), "/app_outcome/counts/",
                            input$inSurveyID,"_",input$inStationID,
                            "_", input$inCounterID,
                            "_counts.csv"),
              row.names = F,
              sep = ",")
  
})


# Showing the last 10 annotations by default in the table
# row.show <- reactive({
#   if (nrow(rv$tablebase) < 11) {return(0)}
#   if (nrow(rv$tablebase) > 10) {return(nrow(rv$tablebase)-10)}
# })

# row.show <- reactive({nrow(rv$tablebase)-10})

# Output of the annotation table
  output$coordinates <- DT::renderDT({
    
    # row.show <- nrow(rv$tablebase)-1
# print(row.show())
    
    if (input$inreviewer == "SIC_matching"){
      DT::datatable(rv$tablebase[,c("time","still_n","feature","station","annotation_time")],
                    selection="single", rownames= FALSE,
                    options = list(dom = 'rti', scrollY = '350px', paging = FALSE, order = list(list(1, 'desc'))
                    )) %>%
        formatStyle(1:5, color="white", backgroundColor = "grey")
      
      
    } else {
          DT::datatable(rv$tablebase[,c("time","still_n","counter_ID","station","annotation_time")],
                  selection="single", rownames= FALSE,
                  options = list(dom = 'rti', scrollY = '350px', paging = FALSE, order = list(list(1, 'desc'))
                                 #, displayStart = 12
                             # ,                  initComplete  = JS('function() {
                             #       $(this.api().table().row(12).node());
                             #       this.api().table().row(12).node().scrollIntoView();
                             #      }')
                             )) %>%
      formatStyle(1:5, color="white", backgroundColor = "grey")
      # formatStyle("still_n", target = "row", color=background)
    # formatStyle(0:5, target = "row", fontWeight = styleEqual(which(rv$tablebase$still == input$inSlider)[1], "bold"))
    # formatStyle("still_n", target = "cell", valueColumns = 3, color = c("orange"))
    }

  })
  
  # Showing the last 10 time annotations by default in the table
  # row.show_time <- reactive({
  #   if (nrow(rvTime$tableTime) < 11) {return(0)}
  #   if (nrow(rvTime$tableTime) > 10) {return(nrow(rvTime$tableTime)-10)}
  # })
  
# Output of the non-countable-time table
  output$non_time_table <- DT::renderDT({
    DT::datatable(rvTime$tableTime[,c("station","counter_ID","start_non_countable","stop_non_countable","minute")], selection="single", rownames= F,
                  options = list(dom = 't', scrollY = '110px', paging = FALSE,
                                 headerCallback = JS("function(thead, data, start, end, display){",
                                                     "  $(thead).remove();",
                                                     "}"))) %>%
      formatStyle(1:6, color="black", backgroundColor = "bisque")
  })
  

  # rvColor <- reactiveValues(df = setNames(data.frame(matrix(ncol = 20, nrow = 3)), c(1:20)))
  
  # rvColor <- eventReactive(input$start, {
  #   rvCol <- setNames(data.frame(matrix(ncol = (as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1), nrow = 3)), c(1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)))
  #   df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
  #   rvCol <- rvCol[1:2,-c((((((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))/2)+1) : ((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))]
  #   rvCol[1,] <- df[1,]
  #   rvCol[2,] <- df[2,]
  #   dataCol_df <- ncol(rvCol)
  #   dataColRng <- 1:dataCol_df
  #   argColRng <- (dataCol_df + 1):(dataCol_df * 2)
  #   rvCol[2, argColRng] <- as.numeric(rvCol[2, dataColRng]) > 30
  #   return(rvCol)
  # })
  # rvColor <- eventReactive(input$confirmTime, {
  #   rvCol <- setNames(data.frame(matrix(ncol = (as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1), nrow = 3)), c(1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)))
  #   df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
  #   rvCol <- rvCol[1:2,-c((((((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))/2)+1) : ((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))]
  #   rvCol[1,] <- df[1,]
  #   rvCol[2,] <- df[2,]
  #   dataCol_df <- ncol(rvCol)
  #   dataColRng <- 1:dataCol_df
  #   argColRng <- (dataCol_df + 1):(dataCol_df * 2)
  #   rvCol[2, argColRng] <- as.numeric(rvCol[2, dataColRng]) > 30
  #   return(rvCol)
  # })
  rvColor <- reactive({
    rvCol <- setNames(data.frame(matrix(ncol = (as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1), nrow = 3)), c(1:(as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)))
    df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
    rvCol <- rvCol[1:2,-c((((((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))/2)+1) : ((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))]
    rvCol[1,] <- df[1,]
    rvCol[2,] <- df[2,]
    dataCol_df <- ncol(rvCol)
    dataColRng <- 1:dataCol_df
    argColRng <- (dataCol_df + 1):(dataCol_df * 2)
    rvCol[2, argColRng] <- as.numeric(rvCol[2, dataColRng]) > 30
    return(rvCol)
  })
  
  # observeEvent({input$start}, {
    # df <- t(rvSeconds$tableNonsecs[,c("minute", "seconds_off")])
    # rvColor$df <- rvColor$df[1:2,-c(21:40)]
    # rvColor$df[1,] <- df[1,]
    # rvColor$df[2,] <- df[2,]
    # dataCol_df <- ncol(rvColor$df)
    # dataColRng <- 1:dataCol_df
    # argColRng <- (dataCol_df + 1):(dataCol_df * 2)
    # rvColor$df[2, argColRng] <- as.numeric(rvColor$df[2, dataColRng]) > 30
  # })
    

  # Output of seconds_off table with red color for minutes with more than 30 seconds kicked out.
  output$non_seconds_table <- DT::renderDT({
    
    DT::datatable(rvColor(), selection="none", rownames= "",
    # DT::datatable(rvColor$df, selection="none", rownames= "",
                  options = list(dom = 't',
                                 headerCallback = JS("function(thead, data, start, end, display){",
                                                     "  $(thead).remove();",
                                                     "}"),
                                 columnDefs = list(list(visible=FALSE, targets=(((((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))/2)+1) : ((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2)))
                                 )) %>%
      formatStyle(columns = 1:((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2),
                  valueColumns = (((((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2))/2)+1):((as.numeric(substring(all.times()[length(jpgsFiles$jpgsNames)], 4, 5)) + 1)*2),
                  backgroundColor = styleEqual(c('0', '1'),
                                               c("lightgreen", "tomato"), default="bisque"),
                  color="black")
    
    # DT::datatable(rvSeconds$tableNonsecs[,c("minute", "seconds_off")], selection = "none",
    #               options = list(dom = 't', pageLength = 20,
                                 # headerCallback = JS("function(thead, data, start, end, display){",
                                 #                     "  $(thead).remove();",
                                 #                     "}"))) %>%
    # formatStyle(columns = 2,
    #             target = "row",
    #             color = JS("value < 31 ? 'green' : value > 30 ? 'red' : 'white'"), backgroundColor = "white",
    #             fontSize = '50%')
    

  })
  
  
  # Matching option
  
  observeEvent({input$inCounterID},{
    
    if(input$inCounterID == "SIC_matching"){
      
      # paste0(volumes_parent[1], "/app_outcome/counts/
      
    
      output$match_pairs <- renderDataTable({
        pre.pairs <- grep(grep(list.files(path = paste0(volumes_parent[1], "/app_outcome/counts"),
                                     pattern = input$inSurveyID),
                               pattern = "counts.csv", value = T),
                          pattern = "SURVEYS_and_COUNTERS|Box|ERROR|SIC_", inv=T, value=T)
        pairs.stn <- sub("*_(.*?) *_.*", "_\\1", sub(paste0(".*", input$inSurveyID, "_ *(.*?) *_counts.*"), "\\1", pre.pairs))
        pairs <- as.data.frame(cbind("file" = as.character(pre.pairs), "st" = as.character(pairs.stn)))
        pairs <- subset(pairs, st %in% unique(pairs$st[duplicated(pairs$st)])) # only if more than 1 counter has counted the station
        pairs[,1:2] <- lapply(pairs[,1:2], as.character)
        pairs2 <- as.data.frame(aggregate(file ~ st , data = pairs, FUN = cbind))
        pairs3 <- NULL
        if (nrow(pairs2) == 1) {
          cur.pair <- cbind(pairs2$st, combinations(n = length(pairs2$file), r = 2, v = pairs2$file, repeats.allowed = F))
          pairs3 <- rbind(pairs3, cur.pair)
        } else {
          for (i in 1:nrow(pairs2)){
            cur.pair <- cbind(pairs2$st[i], combinations(n = length(pairs2$file[[i]]), r = 2, v = pairs2$file[[i]], repeats.allowed = F))
            pairs3 <- rbind(pairs3, cur.pair)
          }
        }
        pairs3 <- as.data.frame(pairs3)
        names(pairs3) <- c("stn", "counter1", "counter2")
        pairs3 <<- pairs3
        rvp$pairs <- pairs3
        DT::datatable(pairs3, colnames = c("stn", "counter1", "counter2"),
                                  selection = list(mode = 'single'),
                      options = list(dom = 'rtip', scrollY = '600px', paging = FALSE))
      })
      
      
      showModal(modalDialog(title ="SIC annotations comparison",
                            HTML("Select the pair you want to check"),
                            br(),
                            br(),
                            DT::dataTableOutput('match_pairs'),
                            footer = tagList(
                              modalButton('Cancel'), 
                              # bsButton('pair.select', 'Select'),
                              bsButton('check.mins', 'Select and check minutes')
                            )))
    }})
  
  rvp <- reactiveValues(selectedRowPair = as.character())
  
  observeEvent(input$check.mins, {

    # Saving the selected row and updating the selectInput
    rvp$selectedRowPair <- req(input$match_pairs_rows_selected)
    
    # Check which minutes are valid
    # seconds_off time
    if(length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                  pattern = paste0(input$inSurveyID,
                                   "_", as.character(rvp$pairs$stn[rvp$selectedRowPair]),
                                   "_", ".*.",
                                   "_seconds_off.csv"))) > 1) { # If there is more than one
      
      showModal(modalDialog(title ="Error: Contact SIC.",
                            HTML("There are more than one .csv file with non_countable_time data for this station.<br>
                                 SIC must check the app_outcome/non_countable_time folder and delete the duplicated .csv files that shouldn't be there.")))
      
    } else if (length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                          pattern = paste0(input$inSurveyID,
                                           "_", as.character(rvp$pairs$stn[rvp$selectedRowPair]),
                                           "_", ".*.",
                                           "_seconds_off.csv"))) == 1) { # if there is one
      
      rvSeconds$tableNonsecs <- read.csv(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                                             pattern = paste0(input$inSurveyID,
                                                              "_", as.character(rvp$pairs$stn[rvp$selectedRowPair]),
                                                              "_", ".*.",
                                                              "_seconds_off.csv")),
                                         colClasses = rep("character", 6))
      
      all_minutes <- unique(as.numeric(as.character(rvSeconds$tableNonsecs$minute)))
      valid_minutes <- as.numeric(as.character(rvSeconds$tableNonsecs$minute))[as.numeric(as.character(rvSeconds$tableNonsecs$seconds_off)) < 31]
      minutes_modal_text <- 'Check that the following minutes are correct. <br> 
                                                Minutes with more than 30" of non-countable time, as saved by the 1st reviewer, have been unticked automatically.'
      
    } else if (length(dir(paste0(as.character(volumes_parent[1]),"/app_outcome/non_countable_time/"), full.names=T,
                          pattern = paste0(input$inSurveyID,
                                           "_", as.character(rvp$pairs$stn[rvp$selectedRowPair]),
                                           "_", ".*.",
                                           "_seconds_off.csv"))) == 0) { # if there is none
      
      rvSeconds$tableNonsecs$seconds_off[1] <- "no_seconds_have_been_removed_from_this_station_yet"
      all_minutes <- 1:20
      valid_minutes <- ""
      minutes_modal_text <- 'There is no non_countable data .csv file. <br> 
                            Select manually the minutes you want to include in the analysis'
    }
    
    showModal(modalDialog(title ="Confirm the valid minutes or edit them manually",
                          HTML(minutes_modal_text),
                          br(),
                          checkboxGroupInput("confirmed_minutes", 
                                             h5("Only the ticked minutes will be used to run the Lin's CCC and the matching code:"), 
                                             choices = all_minutes,
                                             # choices = list("Choice 1" = 1, 
                                             #                "Choice 2" = 2, 
                                             #                "Choice 3" = 3),
                                             selected = valid_minutes),
                          footer = tagList(
                            modalButton('Cancel'),
                            bsButton('pair.run', 'Run comparison')
                            # bsButton('pair.select', 'Select'),
                            
                          )))

  })
  
  


    
    # Actions triggered by Run comparison button
  observeEvent(input$pair.run, {
    
    confirmed_minutes <<- input$confirmed_minutes
    
    withProgress(message = paste0('Runing comparison'), value = 0, {
      setProgress(1/5, detail = paste0("Row selected"))
      

      # Disable the selectinputbuttons
      shinyjs::disable("surveyID")
      shinyjs::disable("counterID")
      shinyjs::disable("reviewer")
      
      shinyjs::disable("pair.run")
      row.match <<- rvp$selectedRowPair
      # Run the matching and Lins code when pressing Run
      counts.folder <<- paste0(volumes_parent[1], "/app_outcome/counts")
      matching.folder <<- paste0(volumes_parent[1], "/matching")


      
      setProgress(2/5, detail = paste0("Running Lin's CCC and matching code"))
      source(paste0(volumes_parent[1], "/matching/matching_annotations.R"))
      
  
      setProgress(3/5, detail = paste0("Plotting matches"))
      # Plotting the matching plot when pressing Run
      output$match_plot <- renderImage({
        list(src = list.files(paste0(volumes_parent[1], "/matching/match_x_625/match_still_36/match_annotations_plots/"), full.names = T),
             contentType = 'image/png',
             width = 600,
             height = 337.5,
             alt = "Waiting for matching plot")}, deleteFile = T)
      
      
      setProgress(4/5, detail = paste0("Showing combined count table"))
      #Creating counts table when pressing Run
      counter1_table <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/counts/",
                                        rvp$pairs$counter1[rvp$selectedRowPair]),
                                 stringsAsFactors = FALSE)
                                 # colClasses = rep("character", n_tablebase))
      counter2_table <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/counts/",
                                        rvp$pairs$counter2[rvp$selectedRowPair]),
                                 stringsAsFactors = FALSE)
                                 # colClasses = rep("character", n_tablebase))
      rv$tablebase <- rbind(counter1_table, counter2_table)
      
      # for 4 kolors
      base.merged <- merge(rv$tablebase, final, all=T)
      base.merged$kol <- as.character(base.merged$kol)
      base.merged$kol[is.na(base.merged$kol)] <- paste0(as.character(base.merged$counter_ID[is.na(base.merged$kol)]), "_no")
      base.merged$feature <- as.factor(base.merged$kol)
      rv$tablebase <- base.merged[names(rv$tablebase)]
      # order by still number
      rv$tablebase <- rv$tablebase[order(as.numeric(rv$tablebase$still_n)),]
      
      removeModal()
      
      setProgress(5/5, detail = paste0("Done: You can now press 'Load stn'"))
      Sys.sleep(2.50)
      # Disable the selectinputbuttons
      shinyjs::disable("surveyID")
      shinyjs::disable("counterID")
      shinyjs::disable("stationID")
      shinyjs::disable("reviewer")
      
  
    })
  })
  
  
  
  # rv$tablebase <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/counts/",
  #                                 input$inSurveyID,"_",input$inStationID,
  #                                 "_", input$inCounterID,
  #                                 "_counts.csv"),
  #                          colClasses = rep("character", n_tablebase))
  
  


} # END OF server


# Run the application 
shinyApp(ui = ui, server = server)