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
      plotOutput("alkanes_plot")
    )
  )
)

##########
# Server #
##########

server <- function(input, output) {

  output$alkanes_plot <- 
    renderPlot(
    read.xlsx(input$datafile$datapath, sheet = 1) %>% 
      ggplot(aes(x = carbons, y = RT_seconds)) + 
      geom_line()+
      geom_point()
    )
   
  
}
shinyApp(ui = ui, server = server)