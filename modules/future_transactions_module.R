# Load required libraries
library(shiny)
library(dplyr)
library(prophet)
library(ggplot2)
library(plotly)
library(shinycssloaders)

# UI for Future Transactions Module
futureTransactionsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             selectInput(ns("forecast_period"), "Forecast Period:",
                         choices = c("1 Month", "3 Months", "6 Months"),
                         selected = "3 Months"),
             radioButtons(ns("category_select"), "Select Category:",
                          choices = c("Income", "Expenses"),
                          selected = "Expenses")
      ),
      column(8,
             withSpinner(plotlyOutput(ns("forecast_plot"), height = "400px"), type = 4, color = "#1f77b4"),
             hr(),
             h4("Historical Decomposition"),
             withSpinner(plotOutput(ns("decomposition_plot"), height = "600px"), type = 4, color = "#ff4785"),
             ),
    )
  )
}

# Server Logic for Future Transactions Module
futureTransactionsModule <- function(input, output, session, transaction_data) {
  ns <- session$ns
  
  # Reactive Forecast Data based on Category
  forecast_data <- reactive({
    req(transaction_data(), input$category_select)
    
    # Filter data by selected category
    filtered_data <- transaction_data() %>%
      filter(Type == input$category_select) %>%
      group_by(Date = as.Date(Date)) %>%
      summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
      rename(ds = Date, y = Total_Amount)
    
    # Ensure there is enough data to train the model
    req(nrow(filtered_data) > 2)
    
    # Train Prophet Model
    model <- prophet(filtered_data)
    forecast_period <- switch(input$forecast_period,
                              "1 Month" = 30,
                              "3 Months" = 90,
                              "6 Months" = 180)
    
    future <- make_future_dataframe(model, periods = forecast_period)
    forecast <- predict(model, future)
    
    # Trim the plot to show only from the most recent date
    most_recent_date <- max(filtered_data$ds)
    forecast <- forecast %>% filter(ds >= most_recent_date)
    list(forecast = forecast, model = model, data = filtered_data)
  })
  
  # Plot Forecast with Highlighted Future Period
  output$forecast_plot <- renderPlotly({
    req(forecast_data())
    forecast <- forecast_data()$forecast
    
    plot_ly(forecast) %>%
      add_lines(x = ~ds, y = ~yhat, name = "Forecast", line = list(color = "#1f77b4")) %>%
      add_ribbons(x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper,
                  name = "Confidence Interval", opacity = 0.2, fillcolor = "#ff4785") %>%
      layout(
        title = paste("Future Transaction Forecast -", input$category_select),
        xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "#eaeaea"),
        yaxis = list(title = "Amount", showgrid = TRUE, gridcolor = "#eaeaea"),
        hovermode = "x"
      )
  })
  
  # Static Decomposition Plot for Historical Data
  output$decomposition_plot <- renderPlot({
    req(transaction_data())
    
    # Prepare and summarize historical data
    historical_data <- transaction_data() %>%
      filter(Type == input$category_select) %>%
      group_by(Date = as.Date(Date)) %>%
      summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
      rename(ds = Date, y = Total_Amount)
    
    # Train a Prophet model on the full historical data
    req(nrow(historical_data) > 2)
    model <- prophet(historical_data)
    forecast <- predict(model, historical_data)
    
    # Create sharper and larger decomposition plot
    components <- prophet_plot_components(model, forecast)
    components + theme_minimal(base_size = 14) +
      theme(
        text = element_text(color = "#333333"),
        panel.grid.major = element_line(color = "#dddddd"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18)
      )
  })
}
