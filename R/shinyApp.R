library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Bird Audio Validation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species_filter", "Select a Species:", choices = NULL, selected = NULL),
      actionButton("save_changes", "Save Changes")
    ),
    mainPanel(
      DTOutput("editable_table")
    )
  )
)



library(shiny)
library(dplyr)
library(DT)
library(tuneR)
library(here)

# Load data using here
file_path <- here("data", "output_test", "output_test_all.csv")

if (!file.exists(file_path)) {
  stop("Data file does not exist at the specified path.")
}

# Load initial dataset
initial_data <- read.csv(file_path)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the data
  rv <- reactiveValues(data = initial_data)
  
  # Populate the species filter dropdown
  observe({
    species_choices <- rv$data %>% 
      select(common_name) %>% 
      distinct() %>% 
      arrange(common_name) %>% 
      pull()
    
    updateSelectInput(session, "species_filter", choices = c("", species_choices))
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    req(input$species_filter)
    
    if (input$species_filter == "") {
      rv$data
    } else {
      rv$data %>% 
        filter(common_name == input$species_filter)
    }
  })
  
  # Render the editable DataTable
  output$editable_table <- renderDataTable({
    data_with_button <- filtered_data() %>%
      mutate(`Play Audio` = '<button class="play-audio">Play Audio</button>')
    
    datatable(
      data_with_button,
      editable = TRUE,
      options = list(
        pageLength = 10#,
        # columnDefs = list(list(
        #   targets = ncol(data_with_button),  # Correctly targets the last column
        #   orderable = FALSE,  # Makes sure the button column is not sortable
        #   render = JS(
        #     "function(data, type, row, meta) {",
        #     "  return data;",  # Use the existing 'Play Audio' column value
        #     "}"
        #   )
        # ))
      ),
      escape = FALSE,  # Allows HTML in the table to be rendered
      rownames = FALSE,
      selection = "none"
    )
  })
  
  # Event listener for the play-audio button click
  observeEvent(input$editable_table_cell_clicked, {
    info <- input$editable_table_cell_clicked
    if (is.null(info$value) || info$col != ncol(filtered_data())) return()  # Check if the Play Audio button was clicked
    
    # Retrieve the file path and start/end times for the selected row
    selected_row <- filtered_data()[info$row, ]
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
  
  # Save changes back to the CSV file
  observeEvent(input$save_changes, {
    updated_data <- filtered_data()
    write.csv(updated_data, file_path, row.names = FALSE)
    showNotification("Changes saved successfully.", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
