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
    
    # Read processing charges data
    process_charge_df <- read_excel(filepath, sheet=2)
    processed_data$processing_charges(process_charge_df)
    
    # Read surcharges data
    # Sort by surcharge type then assign accordingly
    surcharges_df <- read_excel(filepath, sheet=3)
    price_list_surcharge_data <- (surcharges_df %>% 
                              filter(surcharges_df$"Surcharge Type" == "PRICE_LIST"))
    price_list_surcharge_data$"Surcharge Type" <- NULL
    processed_data$price_list_surcharges(price_list_surcharge_data)
    
    process_surcharge_data <- (surcharges_df %>% 
                              filter(surcharges_df$"Surcharge Type" == "PROCESSING"))
    process_surcharge_data$"Surcharge Type" <- NULL
    processed_data$processing_surcharges(process_surcharge_data)
    print(processed_data$price_list_surcharges)
    print(processed_data$processing_surcharges)
  } else {
    showNotification("Please upload a file first.", type = "warning")
  }
}

verify_upload <- function(input, file_path_rv, processed_data, quote_data) {
  req(input$file)
  file_path_rv(input$file$datapath)
  
  processed_data$price_list(NULL)
  processed_data$processing_charges(NULL)
  processed_data$price_list_surcharges(NULL)
  processed_data$processing_surcharges(NULL)
  
  quote_data$selected_items(NULL)
  quote_data$selected_processing(NULL)
}