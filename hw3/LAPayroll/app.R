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
      tableOutput(outputId = "employeeTable")
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
      tableOutput(outputId = "deptEarnTable")
    )
  ),
  
  # Q5: Most Expensive Departments ----
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
      tableOutput(outputId = "deptCostTable")
    )
  )

)

# Server logic ----
server <- function(input, output) {
  
  # Render Plot for Q2 ----
  output$payrollPlot <- renderPlot({
    ggplot(pay) +
      geom_col(aes(x = yr, y = amount, fill = type)) +
      scale_y_continuous(labels = scales::dollar_format("$")) +
      labs(x = "Year", y = "Total Pay")
  })
  
  # Render Table for Q3 ----
  dataInput <- reactive({
    LApayroll %>%
      filter(yr == input$yrQ3) %>%
      arrange(desc(total)) %>%
      select(job, dept, total, base, overtime, other) %>%
      head(input$nEmployee)
  })
  output$employeeTable <- renderTable({
    dataInput()
  })
  
  # Render Table for Q4 ----
  dataInput2 <- reactive({
    if(input$method == "Mean") {
      LApayroll %>%
        filter(yr == input$yrQ4) %>%
        group_by(dept) %>%
        summarise(meanTotal = mean(total), meanBase = mean(base),
                  meanOver = mean(overtime), meanOther = mean(other)) %>%
        arrange(desc(meanTotal)) %>%
        select(dept, meanTotal, meanBase, meanOver, meanOther) %>% 
        gather(meanTotal, meanBase, meanOver, meanOther, key = "type", value = "amount") %>%
        head(input$nDeptEarn)
    } else {
      LApayroll %>%
        filter(yr == input$yrQ4) %>%
        group_by(dept) %>%
        summarise(medTotal = median(total), medBase = median(base),
                  medOver= median(overtime), medOther = median(other)) %>%
        arrange(desc(medTotal)) %>%
        select(dept, medTotal, medBase, medOver, medOther) %>%
        head(input$nDeptEarn)
    }
  })
  output$deptEarnTable <- renderTable({
    dataInput2() 
  })
  
  # Render Table for Q5 ----
  dataInput3 <- reactive({
    LApayroll %>%
      filter(yr == input$yrQ5) %>%
      group_by(dept) %>%
      summarise(sumTotal = sum(total), sumBase = sum(base), 
                sumOver = sum(overtime), sumOther = sum(other), 
                sumCost = sum(cost)) %>%
      arrange(desc(sumCost)) %>%
      select(dept, sumCost, sumTotal, sumBase, sumOver, sumOther) %>%
      head(input$nDeptCost)
  })
  output$deptCostTable <- renderTable({
    dataInput3()
  })
  
}

# Run app ----
shinyApp(ui, server)


