library(shiny)
library(tidyverse)
library(openxlsx)

###################
# User interface  #
###################

ui <- fluidPage(
  titlePanel("KI Calculator"),
  sidebarLayout(
    sidebarPanel("our inputs will go here"),
    mainPanel("the results will go here")
  )
)

##########
# Server #
##########
server <- function(input, output) {}
shinyApp(ui = ui, server = server)