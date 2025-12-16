generate_items_summary_table <- function(selected_items, surcharge_selected) {
  items_df <- selected_items()
  surcharge_label <- paste(surcharge_selected, "cost")
  # No null dataframes, at least 1 dataframe must be non-empty
  if (is.null(items_df) || nrow(items_df) == 0) {
    return(data.frame(Item=character(0), Description=character(0),
                      Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
  }
  if (!surcharge_label %in% names(items_df)) {
    cat("ERROR: Column not found!\n")
    print(surcharge_label)
    return(NULL)
  }
  
  formatted_items_df <- data.frame(
    Item = items_df$Item,
    Description = items_df$Description,
    Quantity = rep(1, nrow(items_df)),
    Amount = items_df[[surcharge_label]]
  )
  
  formatted_items_df$Total <- formatted_items_df$Amount * formatted_items_df$Quantity
  
  return(datatable(formatted_items_df))
}

generate_processing_summary_table <- function(selected_processing_charges, surcharge_selected) {
  processing_df <- selected_processing_charges()
  surcharge_label <- surcharge_selected
  
  if (is.null(processing_df)|| nrow(processing_df) == 0) {
    return(data.frame(Item=character(0), Description=character(0),
                      Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
  }
  if (!surcharge_label %in% names(processing_df)) {
    cat("ERROR: Column not found!\n")
    print(surcharge_label)
    return(NULL)
  }
  
  formatted_processing_df <- data.frame(
    Service = processing_df$Service,
    Description = processing_df$Description,
    Quantity = rep(1, nrow(processing_df)),
    Amount = processing_df[[surcharge_label]]
  )
  
  formatted_processing_df$Total <- formatted_processing_df$Amount * formatted_processing_df$Quantity
  
  return(datatable(formatted_processing_df))
}