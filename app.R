# ---
# title: "OptiFinix"
# author: "Logan Anderson"
# format: Shiny App
# ---

# Deployed at : https://logancanderson.shinyapps.io/OptiFinix/
# Source code at GitHub: https://github.com/logan-c-anderson/OptiFinix



# Ensure all required packages are installed
options(repos = c(CRAN = "https://cran.rstudio.com/"))
packages_needed <- c("shiny", "DT", "shinyWidgets", "shinyjs", "plotly", "lubridate",
                     "dplyr", "ggplot2", "randomForest", "rpart", "rpart.plot",
                     "RColorBrewer", "prophet", "httr", "jsonlite", "shinyBS")

packages_missing <- setdiff(packages_needed, installed.packages()[,"Package"])
if (length(packages_missing) > 0) {
  install.packages(packages_missing)
}

# Load required libraries
library(DT)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(lubridate)
library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(prophet)
library(httr)
library(jsonlite)
library(shinyBS)


# Source the module files
source("modules/summary_module.R")
source("modules/net_balance_module.R")
source("modules/future_transactions_module.R")  # Future Transactions Module
source("modules/plaid_data_summary_module.R")   # Plaid Data Summary Module

# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  
  # Include the custom CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "https://cdn.plaid.com/link/v2/stable/link-initialize.js"),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('plaid_link_token', function(link_token) {
        var handler = Plaid.create({
          token: link_token,
          onSuccess: function(public_token, metadata) {
            Shiny.setInputValue('public_token', public_token);
          },
          onExit: function(err, metadata) {
            if (err != null) {
              console.log('User exited Plaid Link: ', err);
            }
          }
        });
        handler.open();
      });"
    ))
  ),
  
  tags$div(
    style = "display: flex; align-items: center;",
    tags$img(src = "optifinix_logo.png", height = "80px", style = "margin-right: 10px;"), # Adjust size and spacing
    tags$h1("OptiFinix", style = "margin: 0;") # App title
  ),
  useShinyjs(),  # Enable shinyjs functionality
  actionButton("link_button", "Link Your Bank Account"),
  actionButton("populate_data_button", "Populate Local Data"),  # New button to load local data
  br(),
  
  # TabsetPanel with Summary, Net Balance Trends, Predictive Insights, and Future Transactions Tabs
  tabsetPanel(
    tabPanel("Summary",
             fluidRow(
               column(3,
                      pickerInput("year_selector", "Select Year:",
                                  choices = NULL, selected = NULL, 
                                  multiple = FALSE),
                      pickerInput("month_selector", "Select Month:",
                                  choices = month.name, selected = NULL,
                                  multiple = FALSE)
               ),
               column(9,
                      summaryModuleUI("summary")
               )
             )
    ),
    tabPanel("Net Balance Trends",
             fluidRow(
               column(3,
                      pickerInput("net_balance_year_selector", "Select Year:",
                                  choices = NULL, selected = NULL, 
                                  multiple = FALSE),
                      pickerInput("net_balance_month_selector", "Select Month:",
                                  choices = month.name, selected = NULL,
                                  multiple = FALSE)
               ),
               column(9,
                      netBalanceModuleUI("net_balance")
               )
             )
    ),
    tabPanel("Future Transactions",
             futureTransactionsModuleUI("future_transactions")  # Future Transactions UI
    ),
    tabPanel("Plaid Data Summary",
             plaidDataSummaryModuleUI("plaid_data_summary")  # Plaid Data Summary UI
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store local data
  account_data <- reactiveVal()
  
  # Reactive value to store Plaid API data
  plaid_data <- reactiveVal(list(checking = NULL, savings = NULL, credit_card = NULL))
  # Load data from GitHub (.rds format)
  load_data_from_github <- function(url) {
    tryCatch({
      con <- gzcon(url(url, "rb"))  # Open connection to the URL
      data <- readRDS(con)  # Read the RDS file
      close(con)  # Close the connection
      return(data)
    }, error = function(e) {
      showNotification(paste("Error loading data:", url), type = "error")
      return(NULL)  # Return NULL if the data cannot be loaded
    })
  }
  
  # Base URL for GitHub-hosted datasets
  github_base_url <- "https://raw.githubusercontent.com/logan-c-anderson/OptiFinix/main/data/"
  
  # URLs for each dataset (.rds files)
  historical_data_url <- paste0(github_base_url, "synthetic_historical_transactions_data.rds")
  checking_data_url <- paste0(github_base_url, "cleaned_checking_data.rds")
  credit_card_data_url <- paste0(github_base_url, "cleaned_credit_card_data.rds")
  savings_data_url <- paste0(github_base_url, "cleaned_savings_data.rds")
  
  # Function to load GitHub-hosted .rds data
  load_local_data <- function() {
    checking_data <- load_data_from_github(checking_data_url)
    credit_card_data <- load_data_from_github(credit_card_data_url)
    savings_data <- load_data_from_github(savings_data_url)
    
    # Load historical transactions
    historical_data <- load_data_from_github(historical_data_url)
    if (!is.null(historical_data)) {
      historical_data$Date <- mdy(historical_data$Date)  # Ensure Date format
    }
    
    list(
      checking = checking_data,
      credit_card = credit_card_data,
      savings = savings_data,
      historical_transactions = historical_data
    )
  }  
  # # Function to load local data
  # load_local_data <- function() {
  #   checking_data <- read.csv("data/cleaned_checking_data.csv")
  #   credit_card_data <- read.csv("data/cleaned_credit_card_data.csv")
  #   savings_data <- read.csv("data/cleaned_savings_data.csv")
  #   
  #   # Load historical transactions
  #   historical_data <- read.csv("data/synthetic_historical_transactions_data.csv", encoding = "ISO-8859-1")
  #   historical_data$Date <- mdy(historical_data$Date)
  #   
  #   list(
  #     checking = checking_data,
  #     credit_card = credit_card_data,
  #     savings = savings_data,
  #     historical_transactions = historical_data
  #   )
  # }
  
  # Logic for Populate Local Data Button
  observeEvent(input$populate_data_button, {
    shinyjs::show("loading")  # Show spinner
    
    # Load local data
    data <- load_local_data()
    account_data(data)
    
    # Find the most recent date in the dataset
    latest_date <- max(data$historical_transactions$Date, na.rm = TRUE)
    latest_year <- year(latest_date)
    latest_month <- month(latest_date)
    
    # Update selectors for Summary and Net Balances tabs
    updatePickerInput(session, "year_selector",
                      choices = unique(year(data$historical_transactions$Date)),
                      selected = latest_year)
    updatePickerInput(session, "month_selector",
                      choices = month.name,
                      selected = month.name[latest_month])
    
    updatePickerInput(session, "net_balance_year_selector",
                      choices = unique(year(data$historical_transactions$Date)),
                      selected = latest_year)
    updatePickerInput(session, "net_balance_month_selector",
                      choices = month.name,
                      selected = month.name[latest_month])
    
    shinyjs::hide("loading")  # Hide spinner
  })
  
  # Plaid API Integration Logic
  observeEvent(input$link_button, {
    # Ensure client_id and secret are retrieved correctly
    client_id <- Sys.getenv("PLAID_PROD_CLIENT_ID")
    secret <- Sys.getenv("PLAID_PROD_SECRET")
    
    # Debugging Step: Print values to check if they are empty
    print(paste("Client ID:", client_id))
    print(paste("Secret:", secret))
    
    if (client_id == "" || secret == "") {
      showNotification("Plaid API credentials are missing. Check your environment variables.", type = "error")
      return(NULL)
    }
    
    # Request Plaid Link token
    link_token_request <- httr::POST(
      url = "https://production.plaid.com/link/token/create",
      body = list(
        client_id = client_id,
        secret = secret,
        client_name = "OptiFinix",
        country_codes = list("US"),
        language = "en",
        user = list(client_user_id = "unique_user_id"),
        products = list("auth", "transactions")
      ),
      encode = "json"
    )
    
    # Debug API response
    response_content <- content(link_token_request, as = "parsed")
    print(response_content)
    
    if (!is.null(response_content$error_code)) {
      showNotification(paste("Plaid API Error:", response_content$error_message), type = "error")
    } else {
      link_token <- response_content$link_token
      session$sendCustomMessage("plaid_link_token", link_token)
    }
  })
  
  
  observeEvent(input$public_token, {
    # Exchange public token for access token
    access_token_request <- httr::POST(
      url = "https://production.plaid.com/item/public_token/exchange",
      body = list(
        client_id = Sys.getenv("PLAID_CLIENT_ID"),
        secret = Sys.getenv("PLAID_SECRET"),
        public_token = input$public_token
      ),
      encode = "json"
    )
    
    access_token <- content(access_token_request)$access_token
    
    # Fetch account data using access token
    accounts_request <- httr::POST(
      url = "https://production.plaid.com/accounts/balance/get",
      body = list(
        client_id = Sys.getenv("PLAID_CLIENT_ID"),
        secret = Sys.getenv("PLAID_SECRET"),
        access_token = access_token
      ),
      encode = "json"
    )
    
    accounts_data <- content(accounts_request)$accounts
    plaid_data(accounts_data)
  })
  
  # Call existing modules
  callModule(summaryModule, "summary", account_data = account_data, year_input = reactive(input$year_selector), month_input = reactive(input$month_selector))
  callModule(netBalanceModule, "net_balance", account_data = account_data, year_input = reactive(input$net_balance_year_selector), month_input = reactive(input$net_balance_month_selector))
  callModule(futureTransactionsModule, "future_transactions", transaction_data = reactive(account_data()$historical_transactions))
  callModule(plaidDataSummaryModule, "plaid_data_summary", plaid_data = plaid_data)
}

# Run the application
shinyApp(ui = ui, server = server)
