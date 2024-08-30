# app_ui.R
library(shiny)
library(DT)

fluidPage(
  titlePanel("Audio File Validation"),
  
  # Dropdown menu for species selection
  selectInput(
    inputId = "species_filter",
    label = "Select Species to Validate:",
    choices = NULL,  # Updated in the server
    selected = NULL,
    multiple = FALSE
  ),
  
  # DataTable to display and edit data
  dataTableOutput("editable_table"),
  
  # Save changes button
  actionButton("save_btn", "Save Changes")
)
