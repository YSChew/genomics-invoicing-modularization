library(shiny)
library(tidyr)
library(DT)
library(shinyjs)
library(dplyr)
library(stringr)
library(kableExtra)

source("src/ui/invoice.R")  # must define invoicePage() and generateInvoiceTable()
source("src/server-logic.R")
source("src/ui-logic.R")

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  useShinyjs(),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  # Main variables
  current_page <- reactiveVal("main")
  raw_master_spreadsheet_data <- reactiveVal(NULL)
  processed_master_spreadsheet_data <- reactiveVal(NULL)
  file_path <- reactiveVal(NULL)
  invoice_items_data <- reactiveVal(NULL) # Items selected to be added to invoice
  
  process_data(input, output, session, file_path, raw_master_spreadsheet_data, 
               processed_master_spreadsheet_data, invoice_items_data)
  
  # Function containing backend/server logic
  main_server_logic(input, output, session, file_path, 
                processed_master_spreadsheet_data, invoice_items_data, current_page)
  
  # Function containing frontend/ui logic
  main_ui_logic(input, output, session, current_page)
}

shinyApp(ui = ui, server = server)
