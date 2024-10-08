
# preset ------------------------------------------------------------------

library(shiny) 
library(bslib)
library(shinyWidgets) 

library(tidyverse)
library(DT)
library(praise)

library(tuneR)
library(seewave)


# Function to play audio based on filepath, start, and end columns
play_audio <- function(filepath, start, end) { 
  song <- readWave(filepath, from = start - 3, to = end + 3, units = "seconds") #
  play(song, ... = "/play /close") 
}


# Function to view spectrogram
view_spectrogram <- function(filepath, start, end, flim, wl) {
  # Read in the specific segment of the audio file
  song <- readWave(filepath, from = start - 1, to = end + 1, units = "seconds")
  
  # Plot the spectrogram
  seewave::spectro(song, f = song@samp.rate, flim = flim, ovlp = 50,
                   collevels = seq(-40, 0, 1), wl = wl, scale = FALSE)
}



# ui ----------------------------------------------------------------------

ui <- page_sidebar(
  
  # add title
  title = "Bird Audio Validation for BirdNET Threshold Setting",
  
  # add sidebar
  sidebar = sidebar(
    card("Validation File", 
         ## input: validation_file
         fileInput("detection_list", "Detections:", multiple = FALSE, 
                   accept = c("text/csv", 
                              "text/comma-separated-values,text/plain",
                              ".csv"),
                   width = "100%"),
         ## output: target_species
         textOutput("target_species")
    ),
    
    card("Settings",
         ## input: flim, wl
         numericRangeInput("flim", "Frequency range:",
                           min = 1, max = 10, value = c(0, 6)),
         numericInput("wl", "Window length:", value = 512),
    ),
    
    card("Actions",
         ## ui: progress_bar
         uiOutput("progress_bar"),
         ## button: praise_me
         actionButton("praise_me", "Praise me"), 
         ## button: download_data
         downloadButton("download_data", "Download")
    )
    
    
  ),
  
  # add main panel
  card(
    ## output: main_table
    DTOutput("main_table")
  ),
  card(
    ## output: spectrogram
    plotOutput("spectrogram")
  )
)



# server  -----------------------------------------------------------------

server <- function(input, output, session) {

  # reactive values --------------------------------------------------------
  
  # Create a reactiveValues object to store the data
  rv <- reactiveValues()
  
  # Observe the detection_list input and read the data file
  observeEvent(input$detection_list, {
    tryCatch({
      rv$data_display <- read_csv(input$detection_list$datapath)
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  
  # making all the elements on the UI ---------------------------------------

  # render the main table
  output$main_table <- renderDT({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)

    # Render the data table with a play button
    rv$data_display %>%
      mutate(`Spectrogram` = '<button class="spectrogram">Spectrogram</button>',
             `Audio` = '<button class="play-audio">Audio</button>') %>%
      datatable(editable = TRUE,
                escape = FALSE,
                selection = "none",
                options = list(pageLength = 5,
                               columDefs = list(list(targets = ncol(rv$data_display) + 1, orderable = FALSE),
                                                list(targets = ncol(rv$data_display) + 2, orderable = FALSE)))
      )
  })


  # print out target species
  output$target_species <- renderText({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)

    # find out the target species
    paste0("Target: ", rv$data_display %>%
             pull(common_name) %>%
             unique())
  })



  # progress bar
  output$progress_bar <- renderUI({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)

    # Calculate the percentage of T/F values in the data
    percentage <- rv$data_display %>%
      select(validation) %>%
      map_df(~ sum(.x %in% c("T", "F")) / length(.x)) %>%
      pull() %>%
      mean() * 100

    # Create a progress bar
    progressBar(title = "Progress bar:", id = "bar_id", value = percentage)
  })



  # define what will happen for actions -------------------------------------

  ## praise the user when the praise_me button is clicked
  observeEvent(input$praise_me, {
    showNotification(praise(), type = "message")
  })


  ## download the data when the download_data button is clicked
  output$download_data <- downloadHandler(

    filename = function() {
      paste0(rv$data_display %>%
               pull(common_name) %>%
               unique(), "_validated.csv")
    },

    content = function(file) {
      write_csv(rv$data_display, file)
      showNotification("Download successfully.", type = "message")
    }
  )
  
  
  
  ## update the reactive object when data is edited
  observeEvent(input$main_table_cell_edit, {
    
    info <- input$main_table_cell_edit
    
    showNotification(paste0("You made a value change with ", info$value), type = "message")
    
    rv$data_display <<- editData(rv$data_display, info, 'main_table')
    
  })
  
  
  ## show spectrogram when the spectrogram button click
  observeEvent(input$main_table_cell_clicked, {

    info <- input$main_table_cell_clicked

    # Check if the click was on the spectrogram button
    if (is.null(info$value) || info$col != (ncol(rv$data_display) + 1)) return()

    # Retrieve the file path and start/end times for the selected row
    selected_row <- rv$data_display[info$row, ]
    filepath <- selected_row$filepath
    start <- selected_row$start
    end <- selected_row$end

    # show spectrogram
    if (file.exists(filepath)) {
      output$spectrogram <- renderPlot({

        view_spectrogram(filepath = filepath, start = start, end = end,
                         flim = input$flim,
                         wl = input$wl)
      })
    } else {
      showNotification("Audio file not found.", type = "error")
    }
  })


  ## play audio when the play-audio button click
  observeEvent(input$main_table_cell_clicked, {

    info <- input$main_table_cell_clicked

    # Check if the click was on the play-audio button
    if (is.null(info$value) || info$col != (ncol(rv$data_display) + 2)) return()

    # Retrieve the file path and start/end times for the selected row
    selected_row <- rv$data_display[info$row, ]
    filepath <- selected_row$filepath
    start <- selected_row$start
    end <- selected_row$end

    # Play the audio file
    if (file.exists(filepath)) {
      play_audio(filepath = filepath, start = start, end = end)
    } else {
      showNotification("Audio file not found.", type = "error")
    }
  })
  
}



# deploy the Shiny App ----------------------------------------------------

shinyApp(ui = ui, server = server)
