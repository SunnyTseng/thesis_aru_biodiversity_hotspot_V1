
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
view_spectrogram <- function(filepath, start, end, flim, contrast, wl) {
  # Read in the specific segment of the audio file
  song <- readWave(filepath, from = start - 1, to = end + 1, units = "seconds")
  
  # Plot the spectrogram
  spectro(song, f = song@samp.rate, flim = flim, ovlp = 50,
          collevels = seq(-40, 0, 1), wl = wl, contrast = contrast,
          scale = FALSE)
}



# ui ----------------------------------------------------------------------

ui <- page_sidebar(
  
  ## add title
  title = "Bird Audio Validation for BirdNET Threshold Setting",
  
  sidebar = sidebar(
    card(
      ## input: validation_file
      fileInput("detection_list", "Validation file", multiple = FALSE, 
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                width = "100%"),
      
      ## output: to_do_list
      textOutput("target_species")
      ),
    
    card(
      numericRangeInput("flim", "Frequency range:",
                        min = 1, max = 10, value = c(0, 6)),
      numericInput("contrast", "Contrast:", value = 2),
      numericInput("wl", "Window length:", value = 512),
    ),
    
    card(
      ## button: encouragement
      actionButton("praise_me", "Praise me"), 
      
      ## button: save_changes
      actionButton("save_changes", "Save changes"),
    )

    
  ),
  
  ## output: main_table
  DTOutput("main_table"),
  plotOutput("spectrogram")
  
)


# server  -----------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues()
  
  ## read in the file and save it as an reactive object
  observeEvent(input$detection_list, {
    tryCatch({
      rv$data_display <- read_csv(input$detection_list$datapath)
    }, error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
  ## print out the main_table
  output$main_table <- renderDT({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)
    
    # Render the data table with a play button
    rv$data_display %>%
      mutate(`Spectrogram` = '<button class="spectrogram">Spectrogram</button>',
             `Play Audio` = '<button class="play-audio">Play Audio</button>') %>%
      datatable(
        editable = "cell",
        escape = FALSE,
        selection = "none",
        options = list(pageLength = 8)
      )
  })
  
  
  ## print out the target_species
  output$target_species <- renderText({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)
    
    # find out the target species
    paste0("Target: ", rv$data_display %>%
             pull(common_name) %>%
             unique())
  })
  
  
  ## praise the user when the praise_me button is clicked
  observeEvent(input$praise_me, {
    showNotification(praise(), type = "message")
  })
  
  
  ## save changes back to the CSV file
  observeEvent(input$save_changes, {
    write_csv(rv$data_display, "test.csv")
    showNotification("Changes saved successfully.", type = "message")
  })
  
  
  ## update the reactive object when data is edited
  observeEvent(input$main_table_cell_edit, {
    
    info <- input$main_table_cell_edit
    
    showNotification(paste0("You made a value change with ", info$value), type = "message")
    
    rv$data_display <<- editData(rv$data_display, info, 'main_table')
  })
  
  
  
  ## show spectrogram when the spectrogram button click
  observeEvent(input$main_table_cell_clicked, {
    
    info <- input$main_table_cell_clicked
    
    if (is.null(info$value) || info$col != (ncol(rv$data_display) + 1)) return()  # Check if the Play Audio button was clicked
    
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
                         contrast = input$contrast,
                         wl = input$wl)
        
      })
    } else {
      showNotification("Audio file not found.", type = "error")
    }
  })
  
  
  ## play audio when the play-audio button click
  observeEvent(input$main_table_cell_clicked, {
    
    info <- input$main_table_cell_clicked
    
    if (is.null(info$value) || info$col != (ncol(rv$data_display) + 2)) return()  # Check if the Play Audio button was clicked
    
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

  
  
  
#### ChatGPT framework
  
# 
#   # Reactive value to store the data
#   rv <- reactiveValues(data = initial_data)
# 
#   # Populate the species filter dropdown
#   observe({
#     species_choices <- rv$data %>%
#       select(common_name) %>%
#       distinct() %>%
#       arrange(common_name) %>%
#       pull()
# 
#     updateSelectInput(session, "species_filter", choices = c("", species_choices))
#   })
# 
#   # Reactive expression for filtered data
#   filtered_data <- reactive({
#     req(input$species_filter)
# 
#     if (input$species_filter == "") {
#       rv$data
#     } else {
#       rv$data %>%
#         filter(common_name == input$species_filter)
#     }
#   })
# 
#   # Render the editable DataTable
#   output$editable_table <- renderDataTable({
#     data_with_button <- filtered_data() %>%
#       mutate(`Play Audio` = '<button class="play-audio">Play Audio</button>')
# 
#     datatable(
#       data_with_button,
#       editable = TRUE,
#       options = list(
#         pageLength = 10#,
#         # columnDefs = list(list(
#         #   targets = ncol(data_with_button),  # Correctly targets the last column
#         #   orderable = FALSE,  # Makes sure the button column is not sortable
#         #   render = JS(
#         #     "function(data, type, row, meta) {",
#         #     "  return data;",  # Use the existing 'Play Audio' column value
#         #     "}"
#         #   )
#         # ))
#       ),
#       escape = FALSE,  # Allows HTML in the table to be rendered
#       rownames = FALSE,
#       selection = "none"
#     )
#   })
# 
#   # Event listener for the play-audio button click
#   observeEvent(input$editable_table_cell_clicked, {
#     info <- input$editable_table_cell_clicked
#     if (is.null(info$value) || info$col != ncol(filtered_data())) return()  # Check if the Play Audio button was clicked
# 
#     # Retrieve the file path and start/end times for the selected row
#     selected_row <- filtered_data()[info$row, ]
#     filepath <- selected_row$filepath
#     start <- selected_row$start
#     end <- selected_row$end
# 
#     # Play the audio file
#     if (file.exists(filepath)) {
#       song <- readWave(filepath, from = start - 3, to = end + 3, units = "seconds")
#       play(song, ... = "/play /close")
#     } else {
#       showNotification("Audio file not found.", type = "error")
#     }
#   })
# 
#   # Save changes back to the CSV file
#   observeEvent(input$save_changes, {
#     updated_data <- filtered_data()
#     write.csv(updated_data, file_path, row.names = FALSE)
#     showNotification("Changes saved successfully.", type = "message")
#   })
  
}



# deploy the Shiny App ----------------------------------------------------

shinyApp(ui = ui, server = server)
