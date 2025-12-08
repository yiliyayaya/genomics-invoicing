library(shiny)
library(tidyr)
library(DT)
library(shinyjs)
library(dplyr)
library(stringr)
library(kableExtra)

source("src/server-logic.R")
source("src/ui-logic.R")

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  useShinyjs(),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  # Variables
  current_page <- reactiveVal("main")
  file_path <- reactiveVal(NULL)
  
  # List to store data from master spreadsheet
  processed_data <- list()
  processed_data$price_list <- reactiveVal(NULL)
  
  # List to store data collected/selected for quote
  quote_data <- list()
  quote_data$selected_items <- reactiveVal(NULL)
  
  # App Logic
  # Function that converts raw_master_spreadsheet to processed_master_spreadsheet
  process_data(input, output, session, file_path, 
               processed_data, quote_data)
  
  # Function containing backend/server logic
  main_server_logic(input, output, session, file_path, 
                    processed_data, quote_data, current_page)
  
  # Function containing frontend/ui logic
  main_ui_logic(input, output, session, current_page)
}

shinyApp(ui = ui, server = server)
