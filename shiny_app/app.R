library(shiny)
library(tidyverse)
library(openxlsx)

###################
# User interface  #
###################

ui <- fluidPage(
  titlePanel("Kovats Index Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "datafile", label = "upload XLS file", accept = ".xlsx"),
      downloadButton("download", "Download Results")
      
    ),
    mainPanel(
      plotOutput("alkanes_plot"),
      tableOutput("compounds_with_KI")
    )
  )
)

##########
# Server #
##########

server <- function(input, output) {
  
  alkanes <- reactive({
    alkanes <- read.xlsx(input$datafile$datapath, sheet = 1)   
  })
  
  metabolites_of_interest <- reactive({
    metabolites_of_interest <- read.xlsx(input$datafile$datapath, sheet = 2)   
  })
  # plot alkane series from upload
  output$alkanes_plot <- 
    renderPlot(
      read.xlsx(input$datafile$datapath, sheet = 1) %>% 
      ggplot(aes(x = carbons, y = RT_seconds)) + 
      geom_line()+
      geom_point()
    )
   
  ############################
  # Function to calculate KI #
  ############################
  
  fun.KI <- function(my.rt){ 
    RT1 = alkanes() %>% filter(RT_seconds < my.rt) %>% tail(1) %>% .$RT_seconds
    RT2= alkanes() %>% filter(RT_seconds > my.rt) %>% head(1) %>% .$RT_seconds
    N = alkanes()  %>% filter(RT_seconds > my.rt) %>% head(1) %>% .$carbons
    
    KI = round((100 * N)+100*((my.rt - RT1)/(RT2 - RT1)))
    
    return(KI)
  }
  
  
  # Calculate the KI 
  table_processed_data <- reactive({  
    metabolites_of_interest() %>% 
      mutate(calculated_KI = lapply(RT_seconds, fun.KI))
  })
  
  
  # Create the table to display on the screen
  output$compounds_with_KI <-
    renderTable(
      table_processed_data() 
    )
  
  ###############################
  # Download the processed data #
  ###############################
  
  output$download <- downloadHandler(
    
    filename = function() { 
      paste("KI_calculation_Results_", Sys.Date(), ".xlsx", sep="")
    },
    
    content = function(file) {
      
      write.xlsx(apply(table_processed_data(),2,as.character), file)
      
    })
}
shinyApp(ui = ui, server = server)