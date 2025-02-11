# Credit Card Module UI
creditCardModuleUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("credit_card_account_table"))
}


# Credit Card Module Server
creditCardModule <- function(input, output, session, account_data) {
  credit_card_accounts <- reactive({
    subset(account_data()$credit_card, Subtype == "credit")
  })
  
  output$credit_card_account_table <- renderDT({
    datatable(credit_card_accounts(), options = list(pageLength = 5, autoWidth = TRUE))
  })
}
