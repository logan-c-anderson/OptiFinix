summaryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Info Boxes Row
      column(6, uiOutput(ns("current_date_box"))),
      column(6, uiOutput(ns("transactions_count_box")))
    ),
    br(),
    fluidRow(
      # Monthly Financial Overview Row
      column(4, uiOutput(ns("monthly_exps_to_inc_ratio"))),
      column(4, uiOutput(ns("monthly_total_income"))),
      column(4, uiOutput(ns("monthly_total_expenses")))
    ),
    br(),
    fluidRow(
      # Top Categories Section
      column(6, uiOutput(ns("top_expense_categories"))),
      column(6, uiOutput(ns("top_income_categories")))
    ),
    br(),
    fluidRow(
      # Main Content Row
      column(12,
             uiOutput(ns("summary_visualizations_ui")),
             uiOutput(ns("daily_transactions_card"))
      )
    )
  )
}

summaryModule <- function(input, output, session, account_data, year_input, month_input) {
  # Filter data based on selected year and month
  filtered_data <- reactive({
    req(account_data(), year_input(), month_input())  # Ensure inputs are available
    data <- account_data()$historical_transactions
    year_selected <- as.numeric(year_input())
    month_selected <- match(month_input(), month.name)
    data <- data[year(data$Date) == year_selected & month(data$Date) == month_selected, ]
    data
  })
  
  # Aggregate data to find top categories
  top_categories <- reactive({
    data <- filtered_data()
    data_summary <- data %>%
      group_by(Category, Type) %>%
      summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
      ungroup()
    
    list(
      top_expenses = data_summary %>%
        filter(Type == "Expenses") %>%
        arrange(desc(Total_Amount)) %>%
        slice(1:3),
      top_incomes = data_summary %>%
        filter(Type == "Income") %>%
        arrange(desc(Total_Amount)) %>%
        slice(1:3)
    )
  })
  
  # Info Box: Current Date
  output$current_date_box <- renderUI({
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); text-align: center;",
      h4("Current Date"),
      h3(Sys.Date())
    )
  })
  
  # Info Box: Monthly Expense-to-Income Ratio
  output$monthly_exps_to_inc_ratio <- renderUI({
    req(account_data(), year_input(), month_input())  # Ensure required inputs are available
    data <- account_data()$historical_transactions
    
    # Filter data based on the selected year and month
    year_selected <- as.numeric(year_input())
    month_selected <- match(month_input(), month.name)
    data <- data[year(data$Date) == year_selected & month(data$Date) == month_selected, ]
    
    # Calculate total income and expenses
    total_income <- sum(data$Amount[data$Type == "Income"], na.rm = TRUE)
    total_expenses <- sum(data$Amount[data$Type == "Expenses"], na.rm = TRUE)
    
    # Calculate Expense-to-Income Ratio
    exp_to_inc_ratio <- ifelse(total_income > 0, (total_expenses / total_income) * 100, NA)
    
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; text-align: center;",
      h4("Monthly Expense-to-Income Ratio"),
      h3(ifelse(is.na(exp_to_inc_ratio), "No Data Available", paste0(round(exp_to_inc_ratio, 2), "%")))
    )
  })
  
  # Info Box: Total Income for Selected Month
  output$monthly_total_income <- renderUI({
    req(account_data(), year_input(), month_input())  # Ensure required inputs are available
    data <- account_data()$historical_transactions
    
    # Filter data based on the selected year and month
    year_selected <- as.numeric(year_input())
    month_selected <- match(month_input(), month.name)
    data <- data[year(data$Date) == year_selected & month(data$Date) == month_selected, ]
    
    # Calculate total income
    total_income <- sum(data$Amount[data$Type == "Income"], na.rm = TRUE)
    
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; text-align: center;",
      h4("Total Income for", month_input(), year_input()),
      h3(ifelse(is.na(total_income), "No Data Available", paste0("$", formatC(total_income, format = "f", big.mark = ",", digits = 2))))
    )
  })
  
  # Info Box: Total Expenses for Selected Month
  output$monthly_total_expenses <- renderUI({
    req(account_data(), year_input(), month_input())  # Ensure required inputs are available
    data <- account_data()$historical_transactions
    
    # Filter data based on the selected year and month
    year_selected <- as.numeric(year_input())
    month_selected <- match(month_input(), month.name)
    data <- data[year(data$Date) == year_selected & month(data$Date) == month_selected, ]
    
    # Calculate total expenses
    total_expenses <- sum(data$Amount[data$Type == "Expenses"], na.rm = TRUE)
    
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; text-align: center;",
      h4("Total Expenses for", month_input(), year_input()),
      h3(ifelse(is.na(total_expenses), "No Data Available", paste0("$", formatC(total_expenses, format = "f", big.mark = ",", digits = 2))))
    )
  })
  
  # Info Box: Number of Transactions in Selected Year
  year_data <- reactive({
    req(account_data(), year_input())
    data <- account_data()$historical_transactions
    year_selected <- as.numeric(year_input())
    data <- data[year(data$Date) == year_selected, ]
    data
  })
  
  output$transactions_count_box <- renderUI({
    transaction_count <- (nrow(read.csv("data/synthetic_historical_transactions_data.csv", encoding = "ISO-8859-1")) - 1)
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); text-align: center;",
      h4("Total No. of Transactions Tracked"),
      h3(transaction_count)
    )
  })
  
  # Info Box: Top Expense Categories
  output$top_expense_categories <- renderUI({
    top_expenses <- top_categories()$top_expenses
    
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
      h4("Top 3 Expense Categories"),
      tags$ul(
        lapply(seq_len(nrow(top_expenses)), function(i) {
          tags$li(
            paste(top_expenses$Category[i], "- $", 
                  formatC(top_expenses$Total_Amount[i], format = "f", big.mark = ",", digits = 2))
          )
        })
      )
    )
  })
  
  # Info Box: Top Income Categories
  output$top_income_categories <- renderUI({
    top_incomes <- top_categories()$top_incomes
    
    div(
      class = "info-box",
      style = "background-color: #1e2a47; color: #f4f4f4; padding: 15px; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
      h4("Top 3 Income Categories"),
      tags$ul(
        lapply(seq_len(nrow(top_incomes)), function(i) {
          tags$li(
            paste(top_incomes$Category[i], "- $", 
                  formatC(top_incomes$Total_Amount[i], format = "f", big.mark = ",", digits = 2))
          )
        })
      )
    )
  })
  
  # Visualization: Income vs. Expense Bar Chart
  output$summary_visualizations_ui <- renderUI({
    plotlyOutput(session$ns("income_expense_bar_chart"))
  })
  
  output$income_expense_bar_chart <- renderPlotly({
    data <- filtered_data()
    data <- data %>%
      group_by(Date, Type) %>%
      summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
      ungroup()
    
    custom_colors <- c("Income" = "#33CC33",  # Green
                       "Expenses" = "#ff4785",  # Pink
                       "Savings" = "#ff4785")  # Royal Blue
    
    p <- plot_ly(data, 
                 x = ~Date, 
                 y = ~Total_Amount, 
                 color = ~Type, 
                 colors = custom_colors, 
                 type = "bar", 
                 source = session$ns("bar_chart")) %>%
      layout(
        barmode = "group",  # Group bars by day
        title = paste("Income vs. Expenses for", month_input(), year_input()),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Transaction Amount"),
        legend = list(title = list(text = "Transaction Type"))
      )
    
    event_register(p, "plotly_click")  # Register the plotly_click event
    return(p)
  })
  
  
  # Card for Selected Day's Transactions
  # Card for Selected Day's Transactions
  output$daily_transactions_card <- renderUI({
    clicked_data <- event_data("plotly_click", source = session$ns("bar_chart"))
    req(clicked_data)
    clicked_date <- as.Date(clicked_data$x)
    transactions <- filtered_data()
    transactions <- transactions[as.Date(transactions$Date) == clicked_date, ]
    
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
      h4(style = "color: #f4f4f4;", paste("Transactions for", format(clicked_date, "%Y-%m-%d"))),
      card_content
    )
  })
  

}
