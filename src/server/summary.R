generate_items_summary_table <- function(selected_items) {
  items_df <- selected_items()
  # No null dataframes, at least 1 dataframe must be non-empty
  if (is.null(items_df) || nrow(items_df) == 0) {
    return(data.frame(Item=character(0), Description=character(0),
                      Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
  }
  
  summary_table <- items_quote_format(items_df)
  return(datatable(summary_table))
}

generate_processing_summary_table <- function(selected_processing_charges) {
  processing_df <- selected_processing_charges()
  if (is.null(processing_df)|| nrow(processing_df) == 0) {
    return(data.frame(Item=character(0), Description=character(0),
                      Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
  }
  
  summary_table <- processing_quote_format(processing_df)
  return(datatable(summary_table))
}

items_quote_format <- function(items_df) {
  formatted_items_df <- data.frame(
    Item = items_df$Item,
    Description = items_df$Description,
    Quantity = 1,
    Amount = items_df$`Per Reaction Cost ($)`,
    stringsAsFactors = FALSE
  )
  
  formatted_items_df$Total <- formatted_items_df$Amount * formatted_items_df$Quantity
  
  return(formatted_items_df)
}

processing_quote_format <- function(processing_df) {
  formatted_processing_df <- data.frame(
    Service = processing_df$Service,
    Description = processing_df$Description,
    Quantity = 1,
    Amount = processing_df$`Base Price`,
    stringsAsFactors = FALSE
  )
  
  formatted_processing_df$Total <- formatted_processing_df$Amount * formatted_processing_df$Quantity
  
  return(formatted_processing_df)
}

