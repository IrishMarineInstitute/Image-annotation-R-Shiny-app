
# install.packages(c("colorspace", "DT", "exifr", "hms", "jpeg", "magick", "plotrix", "raster", "shiny", "shinyBS", "shinyFiles", "shinyjs", "shinythemes", "shinyWidgets"))

library(colorspace)
library(DT)
library(exifr)
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
                
                

  # TOP ROW: 5 steps to start the app:
  fluidRow(
    column(1,
           uiOutput("surveyID")), # 1st select Survey
    column(1,
           uiOutput("counterID")), # 2nd select Counter
    column(1,
           uiOutput("stationID")), # 3rd select Station
    column(1,
           actionButton("start", "4th: Load stn")), # 4th press button to load station
    column(2,
           tags$h6("5th: press an arrow to start"), # 5th press arrow on keyboard to plot images on screen
           tags$h6("up=PAUSE ; right=PLAY ; left=REWIND")), # app driving instructions
    column(1,
           tags$h6("stills for this station"),
           verbatimTextOutput("jpgnumber")), # check number of images on the folder/station
    column(4,
           tags$h6("Folder selected"),
           verbatimTextOutput("directorypath")), # check folder where the images come from (to check station number is correct)
    tags$style("#directorypath{font-size: 13px}"),

    # License
    column(1, actionButton("lic", "code & license")),
    tags$head(tags$style("#lic{font-size: 12px}")),
    bsModal("lice", "License", "lic", size = "medium",
            HTML("
    Image annotation R Shiny app <br>
    Copyright (C) 2019. Mikel Aristegui * <br><br>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version. <br><br>

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details. <br><br>

    <div>
    <p>You should have received a copy of the GNU Affero General Public License
      along with this program. If not, see <a href=https://www.gnu.org/licenses/>https://www.gnu.org/licenses/</a>.</p>
    </div>
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
    
    # Ancillary data inputs and save button
    column(4,
           fluidRow(
             #hr(),
             column(2, uiOutput("vm")),
             column(2, uiOutput("fq")),
             column(2, uiOutput("pp")),
             column(2, uiOutput("kp")),
             column(2, uiOutput("trawl")),
             column(2, uiOutput("lt"))),
           
           fluidRow(
             column(2, uiOutput("lm")),
             column(2, uiOutput("sl")),
             column(2, uiOutput("fs")),
             column(6, uiOutput("comm"))),
           
           fluidRow(style = "height:35px;",
             
             column(3, uiOutput("nepInN"), actionGroupButtons(c("nepInless","nepInmore"), c("-","+"), direction="horizontal", size="sm")),
             column(3, uiOutput("nepOutN"), actionGroupButtons(c("nepOutless","nepOutmore"), c("-","+"), direction="horizontal", size="sm")),

             
             #br(),
             
             # save button to write ancillary data to .csv file
             column(2,
                    actionButton("anciSave", "Save ancillary data to .csv"), offset = 0)),
           
           hr(),
           
           # Output datatable for the user. To check the burrow systems annotated
            # setting of the datatable
           tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                           color: #ffffff !important;
                           }")),
            # datatable
           DTOutput("coordinates"),
            # Button to delete rows from datatable
           actionButton("delete", "Delete all burrows in current image"))),
  
  
  # MIDDLE 2nd ROW: Reset button, non-countable time input, current time info, speed selector
  
fluidRow(
  
  # Reset button to delete all the annotated images and created .csv files for current Survey & Counter & Station
  column(1,
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
  column(2, textOutput("textTime"),
         actionGroupButtons(c("startTime","stopTime", "confirmTime"), c("start", "stop", "confirm"), # "confirm" button saves "start" and "stop" times into a .csv file
                            direction="horizontal", size="normal"))),
  



# BOTTOM ROW: Slider

fluidRow(
  column(12,
         # display dynamic UI with a slider input to select image number
         uiOutput("slider")))

) # END OF ui




# Define server logic required to draw a histogram
server <- function(input, output, session) {

  volumes <- c(Home = "H:/GitHub/nephrops-burrow-counting-app/Parent_folder/reduced_stn", getVolumes()())
  volumes_parent <- c(Home = "H:/GitHub/nephrops-burrow-counting-app/Parent_folder/", getVolumes()())
  
  
  
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
    selectInput("inCounterID", "2nd: select your ID",
                c("select ID",
                  counters[[surv()]])
    )
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
    selectInput("inStationID", "3rd: select station",
                c("select stn",
                  stations[[surv()]][[input$inCounterID]])
    )
  })

    # Reading the Video operator ID from SURVEYS_and_COUNTERS.csv
  VidOpID <- reactive({
    surveys_counters[surveys_counters["counter"]==input$inCounterID,]$VideoOperatorID
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
  observeEvent({
    input$start
  }, {
    
    if (input$inCounterID != "select ID" & input$inStationID != "select stn"){ # if both counterID and stationID have valid values
      
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
      showModal(modalDialog(title ="Please, select your ID and station number before clicking this button"))
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
  rv <- reactiveValues(tablebase = setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("survey",
                                                                                       "station",
                                                                                       "counter_ID",
                                                                                       "time",
                                                                                       "still_n",
                                                                                       "feature",
                                                                                       "x","y",
                                                                                       "annotation_time",
                                                                                       "VideoOperatorID",
                                                                                       "minute")))
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
                               colClasses = rep("character", 11))
    }
    # ancillary
    if(file_test("-f",
                 paste0(as.character(volumes_parent[1]),
                        "/app_outcome/ancillary/",
                        input$inSurveyID,
                        "_", input$inStationID,
                        "_", input$inCounterID,
                        "_ancillary.csv")) == T) {
      rvTime$tableAncillary <- read.csv(paste0(as.character(volumes_parent[1]),"/app_outcome/ancillary/",
                                          input$inSurveyID,
                                          "_", input$inStationID,
                                          "_", input$inCounterID,
                                          "_ancillary.csv"),
                                   colClasses = rep("character", n_variables))
    } else {
        rvTime$tableAncillary <- data.frame("Nephrops_IN" = 0,
                                            "Nephrops_OUT" = 0)
        }
    
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
                                   colClasses = rep("character", 8))
    }
  })
  
  # ANCILLARY data table
  
  output$vm <- renderUI({
    selectInput("invm", "VAM", choices = list("no","yes"), selected=rvTime$tableAncillary$VAM)
  })
  output$fq <- renderUI({
    selectInput("infq", "FAQ", choices = list("no","yes"), selected=rvTime$tableAncillary$FAQ)
  })
  output$pp <- renderUI({
    selectInput("inpp", "PNP", choices = list("no","yes"), selected=rvTime$tableAncillary$PNP)
  })
  output$kp <- renderUI({
    selectInput("inkp", "KOP", choices = list("no","yes"), selected=rvTime$tableAncillary$KOP)
  })
  output$lm <- renderUI({
    selectInput("inlm", "LMC", choices = list("no","yes"), selected=rvTime$tableAncillary$LMC)
  })
  output$sl <- renderUI({
    selectInput("insl", "Squat", choices = list("no","yes"), selected=rvTime$tableAncillary$squat_lobster)
  })
  output$fs <- renderUI({
    selectInput("infs", "Fish", choices = list("no","yes"), selected=rvTime$tableAncillary$fish)
  })
  output$comm <- renderUI({
    textAreaInput("incomm", "Comments", value=rvTime$tableAncillary$Comments, height = "35px", placeholder="Add comments here")
  })
  
  output$nepInN <- renderUI({
    textOutput(rvTime$tableAncillary$Nephrops_IN)
  })
  output$nepOutN <- renderUI({
    textOutput(rvTime$tableAncillary$Nephrops_OUT)
  })
  
  output$trawl <- renderUI({
    selectInput("intrawl", "Trawl marks", choices = list("no","yes"), selected=rvTime$tableAncillary$trawl_marks)
  })
  output$lt <- renderUI({
    selectInput("inlt", "Litter", choices = list("no","yes"), selected=rvTime$tableAncillary$litter)
  })

  
  # clicking on the save button will create a .csv file with the ancillary data
  observeEvent({input$anciSave},{
    
      rvAncillary$tableAncillary[1,] = c(input$inSurveyID,
                                         input$inStationID,
                                         input$inCounterID,
                                         input$invm, input$infq, input$inpp, input$inkp,
                                         input$inlm, input$insl, input$infs,
                                         (as.numeric(rvTime$tableAncillary$Nephrops_IN) + input$nepInmore - input$nepInless),
                                         (as.numeric(rvTime$tableAncillary$Nephrops_OUT) + input$nepOutmore - input$nepOutless),
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
    
  })
  
    
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
    paste0("log non-countable time: ", substring(textStartStop$start, 4, 8 ), "-", substring(textStartStop$stop, 4, 8))
  })
  # clicking on the confirmTime button will create a .csv file with the new non-countable time section

  observeEvent({input$confirmTime},{
    
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
                                                      substring(textStartStop$start, 7,9):substring(textStartStop$stop, 7,9))
      
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
                           colClasses = rep("character", 8))
      
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
  
  rv$tablebase <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("survey",
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



# Ancillary data for Nephrops in and out

output$nepInN <- renderText({
  paste0("Nephrops IN: ", (as.numeric(rvTime$tableAncillary$Nephrops_IN) + input$nepInmore - input$nepInless))
})
  
output$nepOutN <- renderText({
  paste0("Nephrops OUT: ", (as.numeric(rvTime$tableAncillary$Nephrops_OUT) + input$nepOutmore - input$nepOutless))
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
row.show <- reactive({
  if (nrow(rv$tablebase) < 11) {return(0)}
  if (nrow(rv$tablebase) > 10) {return(nrow(rv$tablebase)-10)}
})

# Output of the annotation table
  output$coordinates <- DT::renderDT({
    DT::datatable(rv$tablebase[,c("station","counter_ID","time","still_n","annotation_time")], selection="single",
              options = list(dom = 'rtip', pageLength = 10, displayStart = row.show())) %>%
      formatStyle(0:5, color="white", backgroundColor = "grey")
  })

  
} # END OF server


# Run the application 
shinyApp(ui = ui, server = server)