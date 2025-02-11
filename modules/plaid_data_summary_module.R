# Load required libraries
library(shiny)
library(DT)

# UI for Plaid Data Summary Module
plaidDataSummaryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, 
             h4("Account Summary"),
             tableOutput(ns("account_summary"))
      ),
      column(6, 
             h4("Detailed Account Data"),
             DTOutput(ns("account_details"))
      )
    )
  )
}

# Server Logic for Plaid Data Summary Module
plaidDataSummaryModule <- function(input, output, session, plaid_data) {
  ns <- session$ns
  
  # Account Summary Table
  output$account_summary <- renderTable({
    req(plaid_data())
    
    # Summarize number of accounts and total balances
    data <- plaid_data()
    summary <- data.frame(
      Account_Type = c("Checking", "Savings", "Credit Card"),
      Number_of_Accounts = c(
        ifelse(is.null(data$checking), 0, nrow(data$checking)),
        ifelse(is.null(data$savings), 0, nrow(data$savings)),
        ifelse(is.null(data$credit_card), 0, nrow(data$credit_card))
      ),
      Total_Balance = c(
        ifelse(is.null(data$checking), 0, sum(data$checking$balance, na.rm = TRUE)),
        ifelse(is.null(data$savings), 0, sum(data$savings$balance, na.rm = TRUE)),
        ifelse(is.null(data$credit_card), 0, sum(data$credit_card$balance, na.rm = TRUE))
      )
    )
    summary
  })
  
  # Detailed Account Data Table
  output$account_details <- renderDT({
    req(plaid_data())
    
    # Combine all account data into one table
    checking <- if (!is.null(plaid_data()$checking)) plaid_data()$checking %>% mutate(Type = "Checking") else NULL
    savings <- if (!is.null(plaid_data()$savings)) plaid_data()$savings %>% mutate(Type = "Savings") else NULL
    credit_card <- if (!is.null(plaid_data()$credit_card)) plaid_data()$credit_card %>% mutate(Type = "Credit Card") else NULL
    
    combined_data <- bind_rows(checking, savings, credit_card) %>%
      select(Type, Account_Name = name, Balance = balance, Available = available_balance)
    
    datatable(combined_data, options = list(pageLength = 10))
  })
}
