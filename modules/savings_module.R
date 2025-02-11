# Savings Module UI
savingsModuleUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("savings_account_table"))
}


# Savings Module Server
savingsModule <- function(input, output, session, account_data) {
  savings_accounts <- reactive({
    subset(account_data()$savings, Subtype == "savings")
  })
  
  output$savings_account_table <- renderDT({
    datatable(savings_accounts(), options = list(pageLength = 5, autoWidth = TRUE))
  })
}
