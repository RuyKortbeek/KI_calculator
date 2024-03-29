library(shiny)
library(tidyverse)
library(openxlsx)
library(httr)
library(ggrepel)

###################
# User interface  #
###################

ui <- fluidPage(
  titlePanel("GC-MS Retention Index Calculator"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("template_download", "Download (example) Template"),
      hr(),
      fileInput(inputId = "datafile", label = "Upload your data", accept = ".xlsx"),
      hr(),
      downloadButton("download", "Download Results")
      
    ),
    mainPanel(
      fluidRow(includeHTML("app_introduction.html")),
      fluidRow(
        tabsetPanel(
          tabPanel("Results", icon = icon("chart-line"),
                   plotOutput("alkanes_plot"),
                   tableOutput("compounds_with_KI")
          ),
          tabPanel("USER HELP",icon = icon("question-circle"),
                   includeHTML("user_help.html")),
          tabPanel("INFO KI / AI Index",icon = icon("info-circle"),
                   includeHTML("KI_AI_info.html"))
          )
      )
      
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
    renderPlot({
      validate(
      need(input$datafile$datapath, "Upload your data first or download the (example) template")
      )
      read.xlsx(input$datafile$datapath, sheet = 1) %>% 
      ggplot(aes(x = carbons, y = retention_time)) + 
      geom_line()+
      geom_point()+
      scale_x_continuous(breaks = seq(0, 40, by = 1))+
      ggtitle("Uploaded alkane series")
    })
  
   
  ###################################
  # Function to calculate KI and AI #
  ###################################
  
  fun.KI <- function(my.rt){ 
    RT1 = alkanes() %>% filter(retention_time < my.rt) %>% tail(1) %>% .$retention_time
    RT2= alkanes() %>% filter(retention_time > my.rt) %>% head(1) %>% .$retention_time
    N = alkanes()  %>% filter(retention_time > my.rt) %>% head(1) %>% .$carbons
    
    KI = round((100 * N) + 100*((log(my.rt) - log(RT1)) / (log(RT2) - log(RT1))))
    
    return(KI)
  }
  
  
  fun.AI <- function(my.rt){ 
    RT1 = alkanes() %>% filter(retention_time < my.rt) %>% tail(1) %>% .$retention_time
    RT2= alkanes() %>% filter(retention_time > my.rt) %>% head(1) %>% .$retention_time
    N = alkanes()  %>% filter(retention_time > my.rt) %>% head(1) %>% .$carbons
    
    AI = round((100 * N)+100*((my.rt - RT1)/(RT2 - RT1)))
    
    return(AI)
  }
  

  
  ######################################
  # Make calculations on input dataset #
  ######################################
  
  table_processed_data <- reactive({  
    metabolites_of_interest() %>% 
      mutate(calculated_KI = lapply(retention_time, fun.KI),
             calculated_AI = lapply(retention_time, fun.AI))
  })
  
  
  # Create the table to display on the screen
  output$compounds_with_KI <-
    renderTable({
        validate(
          need(input$datafile$datapath, "")
        )
      table_processed_data() 
    })
  
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
  #########################
  # Download the template #
  #########################
  
  output$template_download <- downloadHandler(
    
    filename = function() { 
      paste("KI_upload_example_template.xlsx")
    },
    
    content = function(file) {
      
      file.copy("KI_upload_template.xlsx", file)
      
    })
}



shinyApp(ui = ui, server = server)