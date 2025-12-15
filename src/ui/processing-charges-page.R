render_processing_charges_page <- function() {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        # fileInput("file", "Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
        # actionButton("upload_button", "Upload Master Spreadsheet", class = "upload-button"),
        # br(),
        h4("Filter items"),
        selectizeInput("group_filter", "Groups", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All groups")),
        br(),
        actionButton("create_invoice_page", "Create Invoice", class = "invoice-button"),
        br(),
        h4("Processing Surcharges Table"),
        tableOutput("processing_surcharge_table")
      ),
      mainPanel(
        DT::dataTableOutput("processing_charges_table")
      )
    )
  )
}

generate_processing_charge_table <- function(filtered_charges) {
  req(filtered_charges())
  df <- filtered_charges()
  
  # Drop unnecessary columns
  drop <- c("Applications (Brand)", "Item Specific Discount", "Description")
  keep_data <- df[, !names(df) %in% drop]
  
  validate(need(length(keep_data) > 0, "None of the expected columns were found. Check your master’s headers."))
  
  return(datatable(
    keep_data,
    rownames = FALSE,
    options = list(ordering = FALSE, language = list(search = "Search Item:")),
    selection = "multiple"
  ))
}