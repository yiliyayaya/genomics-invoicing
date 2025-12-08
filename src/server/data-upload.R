library(readxl)
library(stringr)
source("src/server/server-helpers.R")

parse_price_list <- function(df) {
  target_col <- "Additional reagent Cost (not incl. in kit)"
  if (target_col %in% names(df)) {
    df[[target_col]][is.na(df[[target_col]])] <- 0
  }
  df
}

#rv stands for reactiveVal
read_spreadsheet_data <- function(filepath, processed_data) {
  if (!is.null(filepath)) {
    if(str_sub(filepath, -5, -1) != ".xlsx") {
      showNotification("Please upload a .xlsx file to continue.", type="warning")
      return()
    }
    
    # Read price list data
    price_list_df <- read_excel(filepath, sheet=1)
    processed_data$price_list(parse_price_list(price_list_df))
  } else {
    showNotification("Please upload a file first.", type = "warning")
  }
}

verify_upload <- function(input, file_path_rv, processed_data_rv, invoice_items_data_rv) {
  req(input$file)
  file_path_rv(input$file$datapath)
  processed_data_rv(NULL)
  invoice_items_data_rv(NULL)
}