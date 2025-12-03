library(shiny)
library(readxl)
library(tidyr)
library(DT)
library(shinyjs)
library(dplyr)
library(stringr)
library(kableExtra)

source("src/ui/invoice.R")  # must define invoicePage() and generateInvoiceTable()
source("src/server-logic.R")

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  useShinyjs(),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  
  current_page <- reactiveVal("main")
  
  raw_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  file_path <- reactiveVal(NULL)
  meta_info <- reactiveVal(list(date = as.character(Sys.Date()), version = "N/A"))
  invoice_items_data <- reactiveVal(NULL)
  edited_invoice_table <- reactiveVal(NULL)
  
  # upload
  observeEvent(input$file, {
    req(input$file)
    file_path(input$file$datapath)
    raw_data(NULL); processed_data(NULL); invoice_items_data(NULL)
  })
  
  observeEvent(input$upload_button, {
    fp <- file_path()
    if (!is.null(fp)) {
      df <- read_excel(fp)
      raw_data(df)
      processed_data(parse_data(df))
      meta_info(list(date = as.character(Sys.Date()), version = "N/A"))
    } else {
      showNotification("Please upload a file first.", type = "warning")
    }
  })
  
  # populate Brand/Product filters
  observeEvent(processed_data(), {
    df <- processed_data()
    if (is.null(df)) return()
    if ("Brand" %in% names(df)) {
      updateSelectizeInput(session, "brand_filter",
                           choices = sort(unique(df$Brand)), server = TRUE)
    }
    if ("Product Name" %in% names(df)) {
      updateSelectizeInput(session, "product_filter",
                           choices = sort(unique(df$`Product Name`)), server = TRUE)
    }
  })
  
  # filtered data
  filtered_data <- reactive({
    req(processed_data())
    df <- processed_data()
    if (!is.null(input$brand_filter) && length(input$brand_filter) > 0) {
      df <- df[df$Brand %in% input$brand_filter, , drop = FALSE]
    }
    if (!is.null(input$product_filter) && length(input$product_filter) > 0) {
      df <- df[df$`Product Name` %in% input$product_filter, , drop = FALSE]
    }
    df
  })
  
  # master summary
  master_summary <- reactive({
    req(processed_data())
    df <- processed_data()
    flags <- guess_type(df)
    data.frame(
      Date = meta_info()$date,
      Version = meta_info()$version,
      `Total items` = sum(flags$is_item, na.rm = TRUE),
      `Physical items` = sum(flags$is_physical, na.rm = TRUE),
      `Processing items` = sum(flags$is_processing, na.rm = TRUE),
      check.names = FALSE
    )
  })
  
  # when rows selected on main table
  observeEvent(input$data_table_rows_selected, {
    rows <- input$data_table_rows_selected
    df <- filtered_data()
    if (!is.null(df) && length(rows) > 0) {
      invoice_items_data(df[rows, , drop = FALSE])
    } else {
      invoice_items_data(NULL)
    }
  })
  
  # navigation
  observeEvent(input$create_invoice_page, {
    if (is.null(invoice_items_data()) || nrow(invoice_items_data()) == 0) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      return()
    }
    edited_invoice_table(invoice_table())
    current_page("invoice_generated")
  })
  observeEvent(input$back_to_main, { current_page("main") })
  
  # extra info
  output$extra_info_table <- renderTable({
    req(processed_data())
    df <- processed_data()
    if (all(c("Internal Extra", "External Extra") %in% names(df))) {
      data.frame(
        `Internal Extra` = unique(na.omit(df[["Internal Extra"]]))[1],
        `External Extra` = unique(na.omit(df[["External Extra"]]))[1]
      )
    } else {
      data.frame(`Internal Extra` = NA, `External Extra` = NA)
    }
  })
  
  output$master_summary_table <- renderTable({
    req(master_summary()); master_summary()
  })
  
  # ui
  output$main_ui <- renderUI({
    if (current_page() == "main") {
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
            actionButton("upload_button", "Upload Master Spreadsheet", class = "upload-button"),
            br(),
            h4("Filter items"),
            selectizeInput("brand_filter", "Brand", choices = NULL, multiple = TRUE,
                           options = list(placeholder = "All brands")),
            selectizeInput("product_filter", "Product", choices = NULL, multiple = TRUE,
                           options = list(placeholder = "All products")),
            br(),
            actionButton("create_invoice_page", "Create Invoice", class = "invoice-button"),
            tags$hr(),
            h4("Master spreadsheet summary"),
            tableOutput("master_summary_table"),
            br(),
            h4("Extra info (Internal/External)"),
            tableOutput("extra_info_table")
          ),
          mainPanel(
            DT::dataTableOutput("data_table")
          )
        )
      )
    } else if (current_page() == "invoice_generated") {
      tryCatch({
        tagList(
          actionButton("back_to_main", "Back", class = "back-button"),
          invoicePage(
            quote_id = generateQuoteID(),
            project_id = "C0000001",
            project_title = "None",
            project_type = "Internal",
            platform = "Xenium"
          )
        )
      }, error = function(e) {
        showNotification(paste("Invoice page failed:", e$message),
                         type = "error", duration = NULL)
        tagList(
          actionButton("back_to_main", "Back", class = "back-button"),
          div(style="padding:1rem; border:1px solid #ccc;",
              h4("Invoice page failed to render"),
              pre(as.character(e$message))
          )
        )
      })
    }
  })
  
  # main table
  output$data_table <- DT::renderDataTable({
    req(filtered_data())
    df <- filtered_data()
    desired <- c("Product Code", "Brand", "Product Category", "Product Name",
                 "per reaction cost", "%PRJ surcharge", "%EXTERNAL surcharge",
                 "Additional reagent Cost (not incl. in kit)")
    keep <- intersect(desired, names(df))
    validate(need(length(keep) > 0, "None of the expected columns were found. Check your master’s headers."))
    datatable(
      df[, keep, drop = FALSE],
      rownames = FALSE,
      options = list(ordering = FALSE, language = list(search = "Search Item:")),
      selection = "multiple"
    )
  })

  
  # invoice with cleaning
  invoice_table <- reactive({
    dat <- invoice_items_data()
    if (is.null(dat) || nrow(dat) == 0) {
      return(data.frame(Item=character(0), Description=character(0),
                        Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
    }
    dat2 <- tryCatch(clean_invoice_cols(dat),
                     error = function(e) {
                       showNotification(paste("Invoice data is malformed:", e$message),
                                        type = "error", duration = NULL)
                       return(NULL)
                     })
    if (is.null(dat2)) {
      return(data.frame(Item=character(0), Description=character(0),
                        Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
    }
    tryCatch(
      new_table <- generateInvoiceTable(dat2),
      error = function(e) {
        showNotification(paste("Failed to build invoice table:", e$message),
                         type = "error", duration = NULL)
        data.frame(Item=character(0), Description=character(0),
                   Quantity=numeric(0), Amount=numeric(0), Total=numeric(0))
      }
    )
    
    output$editable_invoice_table <- DT::renderDataTable({
      new_table
    })
    
    
    output$download_invoice <- downloadHandler(
      filename = function() {
        paste0("Invoice_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # Save a temporary Rmd file
        tempReport <- file.path(tempdir(), "invoice.Rmd")
        file.copy("invoice.Rmd", tempReport, overwrite = TRUE)
        
        pdf_table_data <- new_table
        
        # Parameters to pass into Rmd 
        params <- list(
          date = Sys.Date(),
          quote_id = input$quote_id,
          project_id = input$project_id,
          project_title = input$project_title,
          project_type = input$project_type,
          platform = input$platform,
          table_data = pdf_table_data
        )
        
        # Use tempdir() to save in the default system temp directory
        output_path <- file.path(tempdir(), paste0("Invoice_", Sys.Date(), ".pdf"))
        
        rmarkdown::render(
          tempReport,
          output_file = output_path,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
        # Move the generated file to the 'file' parameter (Shiny will then serve it to the user)
        file.copy(output_path, file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
