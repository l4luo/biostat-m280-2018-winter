setwd(".")

# Load packages ----
if (!require("pacman"))  
  install.packages("pacman", repos = "http://cran.us.r-project.org/")
p_load("tidyverse", "shiny")

# Load data ----
LApayroll <- read_rds("payroll.rds") %>%
  mutate(job = str_wrap(job, width = 30),
         dept = str_wrap(dept, width = 30))

# Create dataset for Q2 ----
pay <- LApayroll %>%
  select(yr, base, overtime, other) %>%
  group_by(yr) %>%
  summarise(`Base Pay` = sum(base, na.rm = TRUE),
            `Overtime Pay` = sum(overtime, na.rm = TRUE),
            `Other Pay` = sum(other, na.rm = TRUE)) %>%
  gather(`Base Pay`, `Overtime Pay`, `Other Pay`, 
         key = "type", value = "amount")
  
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
      plotOutput(outputId = "deptEarnPlot")
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
      geom_col(aes(x = yr, y = amount, 
                   fill = factor(type, levels = c("Other Pay", 
                                                  "Overtime Pay", 
                                                  "Base Pay")))) +
      scale_y_continuous(labels = scales::dollar_format("$")) +
      labs(x = "Year", y = "Total Pay", fill = "Type")
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
  
  # Render Plot for Q4 ----
  dataInput2 <- reactive({
    if(input$method == "Mean") {
      a <- LApayroll %>%
        filter(yr == input$yrQ4) %>%
        group_by(dept) %>%
        summarise(Total = mean(total, na.rm = TRUE), 
                  Base = mean(base, na.rm = TRUE),
                  Over = mean(overtime, na.rm = TRUE), 
                  Other = mean(other, na.rm = TRUE))
    } else {
      a <- LApayroll %>%
        filter(yr == input$yrQ4) %>%
        group_by(dept) %>%
        summarise(Total = median(total, na.rm = TRUE), 
                  Base = median(base, na.rm = TRUE),
                  Over= median(overtime, na.rm = TRUE), 
                  Other = median(other, na.rm = TRUE)) 
    }
    arrange(a, desc(Total)) %>%
      head(input$nDeptEarn) %>%
      gather(Base, Over, Other, key = "type", value = "amount") 
      
  })
  output$deptEarnPlot <- renderPlot({
    dataInput2() %>%
      ggplot() +
        geom_col(aes(x = dept, y = amount, fill = type), position = "dodge") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        labs(x = "Department", y = "Mean/Median Pay") +
        coord_flip()
      
  })
  
  # Render Table for Q5 ----
  dataInput3 <- reactive({
    LApayroll %>%
      filter(yr == input$yrQ5) %>%
      group_by(dept) %>%
      summarise(sumTotal = sum(total), 
                sumBase = sum(base), 
                sumOver = sum(overtime), 
                sumOther = sum(other), 
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


