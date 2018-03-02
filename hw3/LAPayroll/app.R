setwd(".")

# Load packages ----
library(shiny)
library(tidyverse)

# Load data ----
LApayroll <- read_rds("payroll.rds")

# Create dataset for Q2 ----
pay <- LApayroll %>%
  select(yr, base, overtime, other) %>%
  group_by(yr) %>%
  summarise(sumBase = sum(base, na.rm = TRUE),
            sumOver = sum(overtime, na.rm = TRUE),
            sumOther = sum(other, na.rm = TRUE)) %>%
  gather(sumBase, sumOver, sumOther, key = "type", value = "amount")

# User interface ----
ui <- fluidPage(
  
  # Q2: Total Payroll by LA City ----
  titlePanel("Total LA City Payroll By Year"),
  plotOutput(outputId = "payrollPlot"),
  
  # Q3: Highest Earning Employees ----
  titlePanel("Who Earned Most?"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "nEmployee",
                  label = "Choose number of employees to display:",
                  min = 0, max = 30, value = 10), 
      selectInput(inputId = "yrQ3",
                  label = "Choose year:",
                  choices = c("2013", "2014", "2015", "2016", "2017"),
                  selected = "2017")
    ),
    mainPanel(
      tableOutput(outputId = "employeePlot")
    )
  ),  
  
  #Q4: Top-Earning Departments ----
  titlePanel("Which Departments Earn Most?"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "nDeptEarn",
                  label = "Choose number of departments to display:",
                  min = 0, max = 30, value = 5), 
      selectInput(inputId = "yrQ4",
                  label = "Choose year:",
                  choices = c("2013", "2014", "2015", "2016", "2017"),
                  selected = "2017"), 
      radioButtons(inputId = "method", 
                   label = "Choose method:",
                   choices = c("Median", "Mean"),
                   selected = "Median")
    ),
    mainPanel(
      tableOutput(outputId = "deptEarnPlot")
    )
  ),
  
  # Output: Histogram for Q5 ----  
  titlePanel("Which Departments Cost the Most?"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "nDeptCost",
                   label = "Choose number of departments to display:",
                   min = 0, max = 30, value = 5), 
      selectInput(inputId = "yrQ5",
                  label = "Choose year:",
                  choices = c("2013", "2014", "2015", "2016", "2017"),
                  selected = "2017")
    ),
    mainPanel(
      plotOutput(outputId = "deptCostPlot")
    )
  )

)

# Server logic ----
server <- function(input, output) {
  
  output$payrollPlot <- renderPlot({
    ggplot(pay) +
      geom_col(aes(x = yr, y = amount, fill = type)) +
      scale_y_continuous(labels = scales::dollar_format("$")) +
      labs(x = "Year", y = "Total Pay")
  })
  
  # Display Table for Q3
  dataInput <- reactive({
    LApayroll %>%
      select(yr, job, dept, total, base, overtime, other) %>%
      filter(yr == input$yrQ3) %>%
      arrange(desc(total)) %>%
      head(input$nEmployee)
  })
  output$employeePlot <- renderTable({
    dataInput()
  })
  
  
}

# Run app ----
shinyApp(ui, server)


