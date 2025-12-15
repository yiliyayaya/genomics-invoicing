select_processing_charge_rows <- function(input, output, session, filtered_charges) {
  rows <- input$processing_charges_table_rows_selected
  df <- filtered_charges()
  if (!is.null(df) && length(rows) > 0) {
    return(df[rows, , drop = FALSE])
  } else {
    return(NULL)
  }
}