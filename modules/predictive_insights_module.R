# Load required packages
library(shiny)
library(dplyr)
library(prophet)
library(ggplot2)
library(plotly)

# UI for Predictive Insights
predictiveInsightsModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           selectInput(ns("predict_period"), "Select Prediction Period:",
                       choices = c("1 Month", "3 Months", "6 Months"), selected = "1 Month"),
           radioButtons(ns("focus_area"), "Focus on:",
                        choices = c("Income", "Expenses", "Savings"), selected = "Expenses")
    ),
    column(8,
           plotlyOutput(ns("category_spending_plot")),
           tableOutput(ns("category_spending_table"))
    )
  )
}

# Server logic for Predictive Insights
predictiveInsightsModule <- function(input, output, session, historical_data) {
  ns <- session$ns
  
  # Preprocess data (filter for the last 3 months, and forecast range)
  preprocess_data <- reactive({
    req(historical_data())  # Ensure the reactive value is available
    data <- historical_data() %>%
      mutate(Date = as.Date(Date),
             Month = floor_date(Date, "month")) %>%
      group_by(Month, Category, Type) %>%
      summarize(Total = sum(Amount, na.rm = TRUE), .groups = "drop")
    
    # Create a new column "Month_Year" for the x-axis (Month/Year format)
    data$Month_Year <- format(data$Month, "%Y-%m")
    
    # Get the current date
    current_date <- Sys.Date()
    
    # Filter historical data for the current month and previous 2 months
    start_date <- current_date %m-% months(2)  # Two months ago
    filtered_data <- data %>%
      filter(Month >= start_date & Month <= current_date)
    
    return(filtered_data)
  })
  
  # Function to identify fixed categories based on historical behavior
  identify_fixed_categories <- function(data) {
    fixed_categories <- c("Chegg", "Apple Music", "Car Payment", "Rent", "Chat GPT")
    return(fixed_categories)
  }
  
  # Predict Category-Level Spending using Prophet
  predict_spending <- reactive({
    req(preprocess_data(), input$predict_period, input$focus_area)
    data <- preprocess_data()
    
    # Identify fixed categories
    fixed_categories <- identify_fixed_categories(data)
    
    # Filter based on focus area
    focus_data <- data %>%
      filter(Type == input$focus_area)
    
    # Prepare monthly data for Prophet
    monthly_data <- focus_data %>%
      group_by(Month, Category) %>%
      summarize(Total = sum(Total, na.rm = TRUE), .groups = "drop")
    
    # Create a new column "Month_Year" for the x-axis (Month/Year format)
    monthly_data$Month_Year <- format(monthly_data$Month, "%Y-%m")
    
    # Get the current date
    current_date <- Sys.Date()
    
    # Apply Prophet for each category
    category_predictions <- lapply(unique(monthly_data$Category), function(cat) {
      cat_data <- monthly_data %>%
        filter(Category == cat) %>%
        select(Month, Total) %>%
        rename(ds = Month, y = Total)
      
      # Skip categories with less than 2 data points
      if (nrow(cat_data) < 2) {
        # For categories with insufficient data, predict the constant historical average
        future <- data.frame(ds = seq(current_date, by = "month", length.out = switch(input$predict_period,
                                                                                      "1 Month" = 1,
                                                                                      "3 Months" = 3,
                                                                                      "6 Months" = 6)))
        future$yhat <- mean(cat_data$y, na.rm = TRUE)  # Constant value prediction
        future$Category <- cat
        future$Month_Year <- format(future$ds, "%Y-%m")  # Add Month/Year for plotting
        return(future)
      }
      
      # For fixed categories, use constant prediction (no growth)
      if (cat %in% fixed_categories) {
        fixed_value <- mean(cat_data$y, na.rm = TRUE)  # Historical average for fixed categories
        future <- data.frame(ds = seq(current_date, by = "month", length.out = switch(input$predict_period,
                                                                                      "1 Month" = 1,
                                                                                      "3 Months" = 3,
                                                                                      "6 Months" = 6)))
        future$yhat <- fixed_value  # Constant value prediction
        future$Category <- cat
        future$Month_Year <- format(future$ds, "%Y-%m")  # Add Month/Year for plotting
        return(future)
      }
      
      # For variable categories, apply Prophet with growth constraints
      m <- prophet(cat_data, yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
      
      future_periods <- switch(input$predict_period,
                               "1 Month" = 1,
                               "3 Months" = 3,
                               "6 Months" = 6)
      
      future <- make_future_dataframe(m, periods = future_periods, include_history = FALSE)
      forecast <- predict(m, future)
      
      forecast$Category <- cat
      forecast$Month_Year <- format(forecast$ds, "%Y-%m")  # Add Month/Year for plotting
      forecast
    })
    
    # Remove NULL predictions (categories with insufficient data)
    category_predictions <- category_predictions[!sapply(category_predictions, is.null)]
    
    # Combine all category forecasts
    bind_rows(category_predictions)
  })
  
  # Render Category Spending Plot with Plotly
  output$category_spending_plot <- renderPlotly({
    req(predict_spending())
    predictions <- predict_spending()
    historical_data <- preprocess_data()
    
    # Check if data is empty
    validate(
      need(nrow(predictions) > 0, "No data available for predictions.")
    )
    
    # Combine the historical and forecast data for plotting
    combined_data <- bind_rows(historical_data, predictions)
    
    # Ensure that data is aggregated by month and properly formatted for the x-axis
    combined_data_summary <- combined_data %>%
      group_by(Month_Year, Category) %>%
      summarize(Total_Spending = sum(yhat, na.rm = TRUE), .groups = "drop")
    
    # Define 25 colors manually for each category (to ensure differentiation)
    category_colors <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
      "#9edae5", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896",
      "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d",
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
    )
    
    # Create Plotly bar chart for total monthly spending
    plot_ly(
      data = combined_data_summary,
      x = ~Month_Year,  # The Month/Year column (month)
      y = ~Total_Spending,  # Predicted spending amount
      color = ~Category,
      colors = category_colors,
      type = "bar",
      text = ~paste(
        "Category:", Category, "<br>",
        "Month:", Month_Year, "<br>",
        "Predicted Spending: $", round(Total_Spending, 2)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        title = paste("Projected", input$focus_area, "Category Spending"),
        xaxis = list(title = "Month/Year", type = "category"),
        yaxis = list(title = "Predicted Amount ($)")
      )
  })
  
  # Render Category Spending Table
  output$category_spending_table <- renderTable({
    req(predict_spending())
    predictions <- predict_spending()
    
    predictions %>%
      group_by(Category) %>%
      summarize(Total_Predicted = sum(yhat, na.rm = TRUE)) %>%
      arrange(desc(Total_Predicted))
  })
}


