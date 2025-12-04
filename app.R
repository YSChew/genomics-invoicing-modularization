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
  raw_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  file_path <- reactiveVal(NULL)
  invoice_items_data <- reactiveVal(NULL)
  
  process_data(input, output, session, file_path, raw_data, 
               processed_data, invoice_items_data)
  
  server_driver(input, output, session, file_path, 
                processed_data, invoice_items_data, current_page)
  
  ui_driver(input, output, session, current_page)
}

shinyApp(ui = ui, server = server)
