source("src/server/server-helpers.R")

generate_report <- function(input, file, pdf_table_data) {
  # Save a temporary Rmd file
  tempReport <- file.path(tempdir(), "invoice.Rmd")
  file.copy("invoice.Rmd", tempReport, overwrite = TRUE)
  
  # Parameters to pass into Rmd 
  params <- report_params_list(input, pdf_table_data)
  
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