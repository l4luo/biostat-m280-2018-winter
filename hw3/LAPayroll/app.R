setwd(".")

# Load packages ----
library(shiny)
library(tidyverse)

# Load data ----
LApayroll <- read_rds("payroll.rds")


# User interface ----
ui <- fluidPage(
  
  titlePanel("Total LA City Payroll By Year"),
  plotOutput(outputId = "payrollPlot"),
  
  titlePanel("Which Departments Cost the Most?"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "n",
                   label = "Choose number of departments to display:",
                   min = 0, max = 30, value = 5), 
      selectInput(inputId = "yr",
                  label = "Choose year:",
                  choices = c("2013", "2014", "2015", "2016", "2017"),
                  selected = "2017")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "deptCostPlot")
    )
  )
  )

# Server logic ----
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$payrollPlot <- renderPlot({
    LApayroll %>%
      select(yr, base, overtime, other) %>%
      group_by(yr) %>%
      summarise(sumBase = sum(base, na.rm = TRUE),
                sumOver = sum(overtime, na.rm = TRUE),
                sumOther = sum(other, na.rm = TRUE)) %>%
      gather(sumBase, sumOver, sumOther, key = "type", value = "amount") %>%
      ggplot() +
      geom_col(aes(x = yr, y = amount, fill = type)) +
      scale_y_continuous(labels = scales::dollar_format("$")) +
      labs(x = "Year", y = "Total Pay")
  })
}

# Run app ----
shinyApp(ui, server)


