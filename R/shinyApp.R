
# preset ------------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(tuneR)

# Function to play audio based on filepath, start, and end columns
play_audio <- function(filepath, start, end) {
  song <- readWave(filepath, from = start - 3, to = end + 3, units = "seconds")
  play(song)
}


# ui ----------------------------------------------------------------------

ui <- page_sidebar(
  
  ## add title
  title = "Bird Audio Validation",
  
  sidebar = sidebar(
    ## input: validation_file
    fileInput("detection_list", "Choose the detection file", multiple = FALSE, 
              accept = c("text/csv", 
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    ## button: save_changes
    actionButton("save_changes", "Save Changes"),
    
    ## output: species_list_to_do
    tableOutput("species_list_to_do")
  ),
  
  ## output: editable_table
  DTOutput("editable_table")
  
)


# server  -----------------------------------------------------------------

server <- function(input, output, session) {
  
  ## read in the file and save it as an reactive object
  datatable_main <- reactive({
    ## make sure there is a valid input before reading the file
    req(input$detection_list)
    
    ## import dataset
    tryCatch({
      read_csv(input$detection_list$datapath)
    }, error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
  
  ## populate the species that still need to be done
  #! make it filter the species that haven't been processed
  observe({
    ## make the species list from the imported dataset
    species_choices <- datatable_main() %>%
      select(common_name) %>%
      distinct() %>%
      arrange(common_name) %>%
      pull()
    
    output$species_list_to_do <- renderTable(
      tibble(species = species_choices)
    )
  })
  
  ## print out the datatable
  output$editable_table <- renderDataTable(
    datatable_main() %>% mutate(`Play Audio` = '<button class="play-audio">Play Audio</button>'),
    editable = "column",
    filter = "top",
    escape = FALSE, 
    selection = "none",
    rownames = FALSE,
    options = list(pageLength = 8)
  )
  
  
  ## update the reactive object (datatable_main()) with edited column
  #! This is not working now!
  observeEvent(input$editable_table_cell_edit, {
    datatable_main() <<- editData(datatable_main(), input$editable_table_cell_edit, 'editable_table')
  })
  
  
  ## Event listener for the play-audio button click
  observeEvent(input$editable_table_click, {
    
    info <- input$editable_table_click
    
    if (is.null(info$value) || info$col != ncol(filtered_data())) return()  # Check if the Play Audio button was clicked
    
    # Retrieve the file path and start/end times for the selected row
    selected_row <- datatable_main()[info$row, ]
    filepath <- selected_row$filepath
    start <- selected_row$start
    end <- selected_row$end
    
    # Play the audio file
    if (file.exists(filepath)) {
      song <- readWave(filepath, from = start - 3, to = end + 3, units = "seconds")
      play(song, ... = "/play /close")
    } else {
      showNotification("Audio file not found.", type = "error")
    }
  })
  
  
  ## save changes back to the CSV file
  observeEvent(input$save_changes, {
    write.csv(datatable_main(), "test.csv", row.names = FALSE)
    showNotification("Changes saved successfully.", type = "message")
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
