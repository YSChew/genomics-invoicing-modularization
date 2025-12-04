source("src/server/data-upload.R")
source("src/server/selection-page.R")
source("src/server/processing-charges.R")
source("src/server/summary.R")
source("src/server/outputs.R")

process_data <- function(input, output, session, file_path, 
                         raw_data, processed_data, invoice_items_data) {
  # Data upload
  observeEvent(input$file, verify_upload(input, file_path, raw_data, 
                                         processed_data, invoice_items_data))
  
  # Process uploaded data
  observeEvent(input$upload_button, 
               convert_spreadsheet_to_df(file_path(), raw_data, processed_data))
}

server_driver <- function(input, output, session, file_path,
                          processed_data, invoice_items_data, current_page) {
  edited_invoice_table <- reactiveVal(NULL)
  
  # Populate Brand/Product filters list
  observeEvent(processed_data(), populate_brand_product_filters(processed_data, session))
  
  # Filter data
  filtered_data <- reactive(filter_data(input, processed_data))
  
  # Generate master summary
  master_summary <- reactive(generate_master_summary_df(processed_data))
  output$master_summary_table <- renderTable({
    req(master_summary())
    master_summary()
  })
  
  # Generate extra info
  output$extra_info_table <- renderTable(generate_extra_info_table(processed_data))
  
  # Generate main table
  output$data_table <- DT::renderDataTable(generate_main_table(filtered_data))
  
  # Select rows on main table
  observeEvent(input$data_table_rows_selected, 
               invoice_items_data(select_rows(input, output, session, filtered_data)))
  
  # Verify items selected before switching to invoice page
  observeEvent(input$create_invoice_page,{
    if (is.null(invoice_items_data()) || nrow(invoice_items_data()) == 0) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      return()
    }
    edited_invoice_table(invoice_table())
    current_page("invoice_generated")
  })
  
  # Return to main page
  observeEvent(input$back_to_main, { current_page("main") })
  
  # Generate invoice table after cleaning
  invoice_table <- reactive({
    new_table <- clean_invoice_data(invoice_items_data)
    if(verify_empty_df(new_table) || is.null(new_table)) {
      return(new_table)
    } else {
      output$editable_invoice_table <- DT::renderDataTable({ new_table })  
    }
    
    output$download_invoice <- downloadHandler(
      filename = function() { paste0("Invoice_", Sys.Date(), ".pdf") },
      content <- generate_report(input, file, new_table))
  })
}