
install.packages("DT")

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)

# Load dataset
data <- read.csv("academic_performance_dataset_V2.csv")

# UI
dashboard_ui <- fluidPage(
  titlePanel("Academic Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("prog", "Select Program:", choices = unique(data$Prog_Code), selected = NULL, multiple = TRUE),
      selectInput("gender", "Select Gender:", choices = unique(data$Gender), selected = NULL, multiple = TRUE),
      sliderInput("yog", "Select Year of Graduation:",
                  min = min(data$YoG), max = max(data$YoG), value = c(min(data$YoG), max(data$YoG)), step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("CGPA Distribution", plotlyOutput("cgpaPlot")),
        tabPanel("Progression", plotlyOutput("progressPlot")),
        tabPanel("Regression", verbatimTextOutput("regSummary")),
        tabPanel("Data Table", DT::dataTableOutput("table"))
      )
    )
  )
)

# Server
dashboard_server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- data
    if (!is.null(input$prog)) df <- df %>% filter(Prog_Code %in% input$prog)
    if (!is.null(input$gender)) df <- df %>% filter(Gender %in% input$gender)
    df <- df %>% filter(YoG >= input$yog[1], YoG <= input$yog[2])
    df
  })
  
  output$cgpaPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Gender, y = CGPA, fill = Gender)) +
      geom_boxplot() +
      labs(title = "CGPA Distribution by Gender")
    ggplotly(p)
  })
  
  output$progressPlot <- renderPlotly({
    long_data <- melt(filtered_data(), id.vars = "ID_No", measure.vars = c("CGPA100", "CGPA200", "CGPA300", "CGPA400"))
    p <- ggplot(long_data, aes(x = variable, y = value, group = ID_No, color = ID_No)) +
      geom_line(alpha = 0.3) +
      labs(title = "CGPA Progression")
    ggplotly(p)
  })
  
  output$regSummary <- renderPrint({
    model <- lm(CGPA ~ SGPA + CGPA100 + CGPA200 + CGPA300 + CGPA400, data = filtered_data())
    summary(model)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
}

# Run App
shinyApp(ui = dashboard_ui, server = dashboard_server)
