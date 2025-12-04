source("src/ui/main-page.R")
source("src/ui/invoice-page.R")

ui_driver <- function(input, output, session, current_page) {
  # Render UI
  # Note: This can be changed to a switch statement in future for flexibility
  output$main_ui <- renderUI({
    if (current_page() == "main") {
      render_main_page()
    } else if (current_page() == "invoice_generated") {
      render_invoice_page()
    }
  })
}