source("src/ui/invoice.R")

render_invoice_page <- function() {
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