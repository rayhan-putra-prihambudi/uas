## app.R: Shiny Dashboard for Orders Data (DB Connection, ggplot2 only)

library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)

# Function to connect to DB
koneksi_db <- function() {
  dbConnect(
    Postgres(),
    dbname   = Sys.getenv("DB_NAME", "railway"),
    host     = Sys.getenv("DB_HOST", "ballast.proxy.rlwy.net"),
    port     = as.integer(Sys.getenv("DB_PORT", "42732")),
    user     = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "EWMDzrlkGIXjpzKvZIFQqLDfKKFPGtBA")
  )
}

# Load & preprocess data from DB
con <- koneksi_db()
orders <- dbGetQuery(con, "SELECT * FROM orders") %>%
  mutate(
    order_date    = as_date(order_date),
    shipped_date  = as_date(shipped_date),
    required_date = as_date(required_date),
    revenue       = quantity * list_price * (1 - discount)
  )
DBI::dbDisconnect(con)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Orders Dashboard", titleWidth = 280),
  dashboardSidebar(width = 280,
                   sidebarMenu(
                     menuItem("Data",       tabName = "data",    icon = icon("table")),
                     menuItem("Summary",    tabName = "summary", icon = icon("chart-line")),
                     menuItem("By Product", tabName = "product", icon = icon("box")),
                     menuItem("By Store",   tabName = "store",   icon = icon("store")),
                     menuItem("By Staff",   tabName = "staff",   icon = icon("user-tie"))
                   )
  ),
  dashboardBody(
    tabItems(
      # SUMMARY
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("boxOrders"),
                valueBoxOutput("boxRevenue"),
                valueBoxOutput("boxAvgValue")
              ),
              fluidRow(
                box(title = "Monthly Revenue", width = 12, plotOutput("plotRevTS"))
              )
      ),
      # PRODUCT
      tabItem(tabName = "product",
              fluidRow(
                box(title = "Top 10 Products by Qty", width = 6, plotOutput("plotTopProdQty")),
                box(title = "Top 10 Products by Rev", width = 6, plotOutput("plotTopProdRev"))
              )
      ),
      # STORE
      tabItem(tabName = "store",
              fluidRow(
                box(title = "Revenue by Store", width = 12, plotOutput("plotRevStore"))
              )
      ),
      # STAFF
      tabItem(tabName = "staff",
              fluidRow(
                box(title = "Revenue by Staff", width = 12, plotOutput("plotRevStaff"))
              )
      ),
      # DATA
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Raw Orders Data", width = 12, DTOutput("tableData"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Use loaded 'orders' dataset
  df_all <- orders
  
  # Value Boxes
  output$boxOrders <- renderValueBox({
    valueBox(nrow(df_all), "Total Orders", icon = icon("shopping-cart"), color = "purple")
  })
  output$boxRevenue <- renderValueBox({
    rev <- sum(df_all$revenue, na.rm = TRUE)
    valueBox(scales::dollar(rev), "Total Revenue", icon = icon("dollar-sign"), color = "green")
  })
  output$boxAvgValue <- renderValueBox({
    avgv <- if (nrow(df_all) > 0) mean(df_all$revenue) else 0
    valueBox(scales::dollar(avgv), "Avg Order Value", icon = icon("chart-line"), color = "yellow")
  })
  
  # Plots with custom colors
  output$plotRevTS <- renderPlot({
    df <- df_all %>%
      mutate(month = floor_date(order_date, "month")) %>%
      group_by(month) %>% summarise(rev = sum(revenue))
    ggplot(df, aes(x = month, y = rev)) +
      geom_line(color = "#1f77b4", size = 1.2) +
      labs(x = "Month", y = "Revenue") +
      theme_minimal()
  })
  
  output$plotTopProdQty <- renderPlot({
    df <- df_all %>%
      group_by(product_id) %>% summarise(qty = sum(quantity)) %>%
      slice_max(qty, n = 10)
    ggplot(df, aes(x = reorder(product_id, qty), y = qty)) +
      geom_col(fill = "#ff7f0e") + coord_flip() +
      labs(x = "Product ID", y = "Quantity") +
      theme_minimal()
  })
  
  output$plotTopProdRev <- renderPlot({
    df <- df_all %>%
      group_by(product_id) %>% summarise(rev = sum(revenue)) %>%
      slice_max(rev, n = 10)
    ggplot(df, aes(x = reorder(product_id, rev), y = rev)) +
      geom_col(fill = "#2ca02c") + coord_flip() +
      labs(x = "Product ID", y = "Revenue") +
      theme_minimal()
  })
  
  output$plotRevStore <- renderPlot({
    df <- df_all %>% group_by(store_id) %>% summarise(rev = sum(revenue))
    ggplot(df, aes(x = as.factor(store_id), y = rev)) +
      geom_col(fill = "#d62728") + coord_flip() +
      labs(x = "Store ID", y = "Revenue") +
      theme_minimal()
  })
  
  output$plotRevStaff <- renderPlot({
    df <- df_all %>% group_by(staff_id) %>% summarise(rev = sum(revenue))
    ggplot(df, aes(x = as.factor(staff_id), y = rev)) +
      geom_col(fill = "#9467bd") + coord_flip() +
      labs(x = "Staff ID", y = "Revenue") +
      theme_minimal()
  })
  
  # Data table
  output$tableData <- renderDT({
    datatable(df_all, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run App
shinyApp(ui, server)