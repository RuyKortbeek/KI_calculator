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
      fileInput(inputId = "data.file", label = "upload XLS file", accept = ".xlsx")
      
    ),
    mainPanel("the results will go here")
  )
)

##########
# Server #
##########
server <- function(input, output) {}
shinyApp(ui = ui, server = server)