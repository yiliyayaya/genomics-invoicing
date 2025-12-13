render_main_page <- function() {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
        actionButton("upload_button", "Upload Master Spreadsheet", class = "upload-button"),
        br(),
        h4("Filter items"),
        selectizeInput("brand_filter", "Brand", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All brands")),
        selectizeInput("item_filter", "Item", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All items")),
        selectizeInput("category_filter", "Category", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All categories")),
        br(),
        actionButton("create_invoice_page", "Create Invoice", class = "invoice-button"),
        tags$hr(),
        h4("Master spreadsheet summary"),
        tableOutput("master_summary_table"),
        br(),
        h4("Surcharges Table"),
        tableOutput("extra_info_table")
      ),
      mainPanel(
        DT::dataTableOutput("data_table")
      )
    )
  )
}

generate_extra_info_table <- function(surcharge_table) {
    req(surcharge_table())
    df <- surcharge_table()
    
    if(nrow(df) > 0) {
      # Initialize empty dataframe
      columns = c("Surcharge Label", "Surcharge Amount")
      extra_info_table <- data.frame(matrix(nrow=0, ncol=length(columns)))
      colnames(extra_info_table) = columns
      extra_info_table$`Surcharge Label` <- as.character(extra_info_table$`Surcharge Label`)
      extra_info_table$`Surcharge Amount` <- as.double(extra_info_table$`Surcharge Amount`)
      
      #Iterate through rows and add to table
      for(i in 1:nrow(df)) {
        surcharge_label <- df$`Surcharge Label`[i]
        surcharge_amount <- df$`Surcharge Amount`[i]
        
        extra_info_table <- extra_info_table %>% add_row(`Surcharge Label` = surcharge_label,
                                                         `Surcharge Amount` = surcharge_amount)
      }
      
      return(extra_info_table)
    } else {
      return(data.frame(`Surcharge Label`=NA, `Surcharge Amount`=NA))
    }
}

generate_main_table <- function(filtered_data) {
  req(filtered_data())
  df <- filtered_data()
  
  # Drop unnecessary columns
  drop <- c("Constant Cost", "Item Specific Discount", "Description")
  keep_data <- df[, !names(df) %in% drop]
  
  validate(need(length(keep_data) > 0, "None of the expected columns were found. Check your master’s headers."))
  
  # Move additional cost to last column for readability
  if("Additional reagent Cost (not incl. in kit)" %in% names(keep_data)) {
    keep_data <- keep_data %>% relocate(`Additional reagent Cost (not incl. in kit)`, .after = last_col())
  }
    
  return(datatable(
    keep_data,
    rownames = FALSE,
    options = list(ordering = FALSE, language = list(search = "Search Item:")),
    selection = "multiple"
  ))
}