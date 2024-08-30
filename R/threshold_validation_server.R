# app_server.R
library(shiny)
library(DT)
library(dplyr)
library(tuneR)
library(here)

# Path to your CSV file
csv_file_path <- here("data", "output_test", "output_test_all.csv")

# Load data from CSV
load_data <- function() {
  if (file.exists(csv_file_path)) {
    read.csv(csv_file_path, stringsAsFactors = FALSE)
  } else {
    stop("CSV file does not exist at the specified path.")
  }
}

# Save data to CSV
save_data <- function(data) {
  write.csv(data, csv_file_path, row.names = FALSE)
}

# Function to play audio based on filepath, start, and end columns
play_audio <- function(filepath, start, end) {
  song <- readWave(filepath, from = start - 3, to = end + 3, units = "seconds")
  play(song)
}

server <- function(input, output, session) {
  # Reactive value to store the data
  rv <- reactiveValues(data = load_data())
  
  # Update the species dropdown choices
  observe({
    species_choices <- unique(rv$data$common_name)
    updateSelectInput(session, "species_filter", choices = species_choices, selected = species_choices[1])
  })
  
  # Filtered data based on selected species
  filtered_data <- reactive({
    if (is.null(input$species_filter)) {
      rv$data
    } else {
      rv$data %>%
        filter(common_name == input$species_filter)
    }
  })
  
  # Render the DataTable with filtered data
  output$editable_table <- renderDataTable({
    data_with_button <- filtered_data() %>%
      mutate(`Play Audio` = sprintf('<button class="play-audio" data-filepath="%s" data-start="%s" data-end="%s">Play Audio</button>',
                                    filepath, start, end))
    
    datatable(
      data_with_button,
      editable = TRUE,
      options = list(pageLength = 10),
      callback = JS(
        "table.on('click.dt', 'button.play-audio', function() {",
        "  var data = $(this).data();",
        "  Shiny.setInputValue('play_audio', {filepath: data.filepath, start: parseFloat(data.start), end: parseFloat(data.end)});",
        "});"
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "none"
    )
  })
  
  # Observe play_audio input and play the audio
  observeEvent(input$play_audio, {
    req(input$play_audio$filepath)
    play_audio(input$play_audio$filepath, input$play_audio$start, input$play_audio$end)
  })
  
  # Update data when a cell is edited
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    # Update the reactive data
    rv$data[info$row, info$col] <- info$value
  })
  
  # Save data when the save button is clicked
  observeEvent(input$save_btn, {
    save_data(rv$data)
    showNotification("Changes saved!", type = "message")
  })
}
