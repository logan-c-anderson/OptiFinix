netBalanceModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
           plotlyOutput(ns("net_balance_chart")),  # Placeholder for the line chart
           uiOutput(ns("daily_transactions_card"))  # Card for clicked day transactions
    )
  )
}

netBalanceModule <- function(input, output, session, account_data, year_input, month_input) {
  # Filter data based on selected year and month
  filtered_data <- reactive({
    req(account_data(), year_input(), month_input())  # Ensure inputs are available
    data <- account_data()$historical_transactions
    year_selected <- as.numeric(year_input())
    month_selected <- match(month_input(), month.name)
    data <- data[year(data$Date) == year_selected & month(data$Date) == month_selected, ]
    data
  })
  
  # Calculate daily net balance
  daily_net_balance <- reactive({
    data <- filtered_data()
    data_summary <- data %>%
      group_by(Date) %>%
      summarize(
        Total_Income = sum(Amount[Type == "Income"], na.rm = TRUE),
        Total_Expenses = sum(Amount[Type == "Expenses"], na.rm = TRUE),
        Net_Balance = Total_Income - Total_Expenses
      ) %>%
      ungroup()
    data_summary$Date <- as.Date(data_summary$Date)  # Ensure Date format
    data_summary
  })
  
  # Reactive for storing the clicked date
  clicked_date <- reactiveVal(NULL)
  
  # Update clicked date based on chart interaction
  observeEvent(event_data("plotly_click", source = session$ns("line_chart")), {
    clicked <- event_data("plotly_click", source = session$ns("line_chart"))
    if (!is.null(clicked)) {
      clicked_date(as.Date(clicked$x))
    }
  })
  
  # Render Net Balance Line Chart
  output$net_balance_chart <- renderPlotly({
    data <- daily_net_balance()
    
    # Create line chart
    p <- plot_ly(data, 
                 x = ~Date, 
                 y = ~Net_Balance, 
                 type = "scatter", 
                 mode = "lines+markers", 
                 line = list(color = "#1f77b4"),  # Blue line for net balance
                 text = ~paste(
                   "Date:", format(Date, "%Y-%m-%d"), "<br>",
                   "Income: $", formatC(Total_Income, format = "f", big.mark = ",", digits = 2), "<br>",
                   "Expenses: $", formatC(Total_Expenses, format = "f", big.mark = ",", digits = 2), "<br>",
                   "Net Balance: $", formatC(Net_Balance, format = "f", big.mark = ",", digits = 2)
                 ),
                 hoverinfo = "text",
                 source = session$ns("line_chart")) %>%
      layout(
        title = paste("Daily Net Balance Trends for", month_input(), year_input()),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Net Balance ($)", zeroline = TRUE),
        margin = list(l = 60, r = 20, t = 50, b = 50)
      )
    
    p <- event_register(p, "plotly_click")  # ✅ Fix: Ensure event_register is applied to `p`
    
    return(p)
  })
  
  # Render a card for the selected day's transactions
  output$daily_transactions_card <- renderUI({
    clicked_data <- clicked_date()  # ✅ Get clicked date reactively
    req(clicked_data)
    
    transactions <- filtered_data()
    transactions <- transactions[as.Date(transactions$Date) == clicked_data, ]
    
    card_content <- DT::datatable(
      transactions[, c("Date", "Category", "Details", "Amount")], 
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 't',  # Simple table without pagination or search
        initComplete = JS(
          "function(settings, json) {
          $(this.api().table().body()).css({
            'color': '#ffffff'  // Set table body text color to white
          });
          $(this.api().table().header()).css({
            'color': '#ffffff',  // Set table header text color to white
            'background-color': '#1e2a47'  // Match header background to card
          });
        }"
        )
      ),
      rownames = FALSE,
      colnames = c("Date", "Category", "Details", "Amount ($)")
    )
    
    div(
      class = "card mt-4",
      style = "padding: 20px; background-color: #1e2a47; border: 1px solid #ccc; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
      h4(style = "color: #f4f4f4;", paste("Transactions for", format(clicked_data, "%Y-%m-%d"))),
      card_content
    )
  })
}
