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
      fileInput(inputId = "datafile", label = "upload XLS file", accept = ".xlsx")
      
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
    RT1 = read.xlsx(input$datafile$datapath, sheet = 1)  %>% filter(RT_seconds < my.rt) %>% tail(1) %>% .$RT_seconds
    RT2= read.xlsx(input$datafile$datapath, sheet = 1)  %>% filter(RT_seconds > my.rt) %>% head(1) %>% .$RT_seconds
    N = read.xlsx(input$datafile$datapath, sheet = 1)  %>% filter(RT_seconds > my.rt) %>% head(1) %>% .$carbons
    
    KI = round((100 * N)+100*((my.rt - RT1)/(RT2 - RT1)))
    
    return(KI)
  }
  
  output$compounds_with_KI <-
    renderTable(
      read.xlsx(input$datafile$datapath, sheet = 2) %>% 
        mutate(calculated_KI = lapply(RT_seconds, fun.KI))
    )
    
}
shinyApp(ui = ui, server = server)