# Checking Module UI
checkingModuleUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("checking_account_table"))
}

checkingModule <- function(input, output, session, account_data) {
  checking_accounts <- reactive({
    subset(account_data()$checking, Subtype == "checking")
  })
  
  output$checking_account_table <- renderDT({
    datatable(checking_accounts(), options = list(pageLength = 5, autoWidth = TRUE))
  })
}