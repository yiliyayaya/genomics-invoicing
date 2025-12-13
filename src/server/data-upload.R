library(readxl)
library(stringr)
source("src/server/server-helpers.R")

parse_price_list <- function(df) {
  df_names <- names(df)
  
  df <- df %>% drop_na("Item") 
    
  # Fill empty values
  per_reaction_col <- "Per Reaction Cost ($)"
  if (per_reaction_col %in% df_names) {
    df[[per_reaction_col]][is.na(df[[per_reaction_col]])] <- 0
  }
  
  add_cost_col <- "Additional reagent Cost (not incl. in kit)"
  if (add_cost_col %in% df_names) {
    df[[add_cost_col]][is.na(df[[add_cost_col]])] <- 0
  }
  
  const_cost_col <- "Constant Cost"
  if (const_cost_col %in% df_names) {
    df[[const_cost_col]][is.na(df[[const_cost_col]])] <- FALSE
  }
  return(df)
}

read_process_spreadsheet_data <- function(filepath, processed_data) {
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
    processed_data$price_list_surcharges(price_list_surcharge_data)
    
    process_surcharge_data <- (surcharges_df %>% 
                              filter(surcharges_df$"Surcharge Type" == "PROCESSING"))
    processed_data$processing_surcharges(process_surcharge_data)
    
    new_price_list <- calculate_new_price_list(processed_data$price_list(), 
                             processed_data$price_list_surcharges())
    processed_data$price_list(new_price_list)
    
    # Read discounts data
    discounts_df <- read_excel(filepath, sheet=4)
    processed_data$brand_discounts(discounts_df)
    
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
  processed_data$brand_discounts(NULL)
  
  quote_data$selected_items(NULL)
  quote_data$selected_processing(NULL)
}