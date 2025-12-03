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
  meta_info <- reactiveVal(list(date = as.character(Sys.Date()), version = "N/A"))
  invoice_items_data <- reactiveVal(NULL)
  edited_invoice_table <- reactiveVal(NULL)
  
  # upload
  observeEvent(input$file, { verify_upload(input, file_path, raw_data, 
                                           processed_data, invoice_items_data) })
  
  observeEvent(input$upload_button, 
               convert_spreadsheet_to_df(file_path(), raw_data, processed_data, meta_info))
  
  # populate Brand/Product filters
  observeEvent(processed_data(), populate_brand_product_filters(processed_data, session))
  
  # filtered data
  filtered_data <- reactive(filter_data(input, processed_data))
  
  # master summary
  master_summary <- reactive(generate_master_summary_df(processed_data, meta_info))
  
  # when rows selected on main table
  observeEvent(input$data_table_rows_selected, 
               { invoice_items_data(select_rows(input, output, session, filtered_data))})
  
  # navigation
  observeEvent(input$create_invoice_page,{
    if (is.null(invoice_items_data()) || nrow(invoice_items_data()) == 0) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      return()
    }
    edited_invoice_table(invoice_table())
    current_page("invoice_generated")
  })
  observeEvent(input$back_to_main, { current_page("main") })
  
  # extra info
  output$extra_info_table <- renderTable(generate_extra_info_table(processed_data))
  
  output$master_summary_table <- renderTable({
    req(master_summary())
    master_summary()
  })
  
  # ui
  output$main_ui <- renderUI({
    if (current_page() == "main") {
      render_main_page()
    } else if (current_page() == "invoice_generated") {
      render_invoice_page()
    }
  })
  
  # main table
  output$data_table <- DT::renderDataTable(generate_main_table(filtered_data))
  
  # invoice with cleaning
  invoice_table <- reactive({
    new_table <- clean_invoice_data(invoice_items_data)
    if(verify_empty_df(new_table) || is.null(new_table)) {
      return(new_table)
    } else {
      output$editable_invoice_table <- DT::renderDataTable({ new_table })  
    }
    
    output$download_invoice <- downloadHandler(
      filename = function() { paste0("Invoice_", Sys.Date(), ".pdf") },
      content <- generate_report(input, file, new_table)
    )
  })
}

shinyApp(ui = ui, server = server)
