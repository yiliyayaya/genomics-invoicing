source("src/server/data-upload.R")
source("src/server/selection-page.R")
source("src/server/processing-charges.R")
source("src/server/summary.R")
source("src/server/outputs.R")

process_data <- function(input, output, session, file_path, 
                         processed_data, quote_data) {
  # Data upload
  observeEvent(input$file, verify_upload(input, file_path, processed_data, 
                                         quote_data))
  
  # Process uploaded data
  observeEvent(input$upload_button, 
               read_process_spreadsheet_data(file_path(), processed_data))
}

main_server_logic <- function(input, output, session, file_path,
                          processed_data, quote_data, current_page) {
  edited_invoice_table <- reactiveVal(NULL)
  
  
  # PRICE LIST PAGE LOGIC =======================================================================================
  # Populate filters
  observeEvent(processed_data$price_list(), populate_selection_page_filters(processed_data$price_list, session))
  
  # Filter data
  filtered_data <- reactive(filter_data(input, processed_data$price_list))
  
  # Generate master summary
  master_summary <- reactive(generate_master_summary_df(processed_data$price_list))
  
  output$master_summary_table <- renderTable({
    req(master_summary())
    master_summary()
  })
  
  # Generate extra info
  output$price_list_surcharges_table <- renderTable(generate_surcharge_reference(processed_data$price_list_surcharges))
  
  # Generate main table
  output$price_list_main_table <- DT::renderDataTable(generate_main_table(filtered_data))
  
  # Select rows on main table
  observeEvent(input$data_table_rows_selected, 
               quote_data$selected_items(select_rows(input, output, session, filtered_data)))
  
  observeEvent(input$to_processing_charges_page,{ current_page("processing_charges") })
  
  # PROCESSING PAGE LOGIC =======================================================================================
  
  # Verify items selected before switching to invoice page
  observeEvent(input$create_invoice_page,{
    if (is.null(quote_data$selected_items()) || nrow(quote_data$selected_items()) == 0) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      return()
    }
    edited_invoice_table(invoice_table())
    current_page("invoice_generated")
  })
  
  output$processing_surcharge_table <- renderTable(generate_surcharge_reference(processed_data$processing_surcharges))
  
  output$processing_charges_table <- DT::renderDataTable(generate_processing_charge_table(processed_data$processing_charges))
  
  # INVOICE PAGE LOGIC =======================================================================================
  
  # Return to main page
  observeEvent(input$back_to_main, { current_page("main") })
  
  # Generate invoice table after cleaning
  invoice_table <- reactive({
    new_table <- clean_invoice_data(quote_data$selected_items)
    if(verify_empty_df(new_table) || is.null(new_table)) {
      return(new_table)
    } else {
      output$editable_invoice_table <- DT::renderDataTable({ new_table })  
    }
    
    output$download_invoice <- downloadHandler(
      filename = function() { paste0("Invoice_", Sys.Date(), ".pdf") },
      content = function(file) { generate_report(input, file, new_table) }
    )
  })
}