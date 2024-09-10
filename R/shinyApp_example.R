library(shiny)
library(DT)

Admit<-c("Admitted","Rejected","Admitted", "Rejected", "Admitted", "Rejected", "Admitted",
         "Rejected","Admitted", "Rejected", "Admitted","Rejected","Admitted", "Rejected","Admitted","Rejected", "Admitted", "Rejected",
         "Admitted","Rejected", "Admitted" ,"Rejected","Admitted", "Rejected")
Gender<-c("Male","Male","Female","Female", "Male",   "Male",   "Female", "Female", "Male","Male","Female","Female",
          "Male","Male","Female","Female","Male",   "Male",   "Female", "Female","Male","Male","Female","Female")
Dept<-c( "A","A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "C", "D", "D", "D", "D", "E", "E", "E", "E", "F", "F", "F", "F")
Freq<-c("512", "313",  "89",  "19", "353", "207",  "17",   "8", "120", "205", "202", "391", "138", "279", "131", "244",  "53", "138",
        "94", "299",  "22", "351",  "24", "317")

banking.df<-data.frame(Admit,Gender,Dept, Freq,stringsAsFactors = FALSE)
d1 = banking.df
d1$Date = Sys.time() + seq_len(nrow(d1))



# ui ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("UC Berkley Admissions"),
  
  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Sample Bank", 
               
               DT::dataTableOutput("banking.df_data"),
               br(),
               actionButton("viewBtn","View"),
               br(),
               actionButton("saveBtn","Save"),
               br(),
               DT::dataTableOutput("updated.df")
      ))))



# server ------------------------------------------------------------------

server <- function(input, output) {
  
  
  output$banking.df_data <- renderDataTable(
    d1,selection = 'none', editable = TRUE, 
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    ),
    
    class = "display"
  )
  
  
  observeEvent(input$banking.df_data_cell_edit, {
    
    info <- input$banking.df_data_cell_edit
    
    str(info)
    
    d1 <<- editData(d1, input$banking.df_data_cell_edit, 'banking.df_data')
    
    #d1[input$banking.df_data_cell_edit$row,input$banking.df_data_cell_edit$col] <<- input$banking.df_data_cell_edit$value
  })
  
  
  view_fun<-eventReactive(input$viewBtn,{
    if(is.null(input$saveBtn)||input$saveBtn==0)
    {
      returnValue()
    }
    else
    {
      DT::datatable(d1,selection = 'none')
    }
    
  })
  
  
  observeEvent(input$saveBtn,{
    write.csv(d1,'test.csv')
  })
  
  output$updated.df<-renderDataTable({
    view_fun()
  }
  )
}


shinyApp(ui, server)