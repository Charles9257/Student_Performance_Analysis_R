


library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(dplyr)
library(corrplot)

# Load data
data <- read.csv("students_performance.csv")
colnames(data) <- make.names(colnames(data))  # clean column names

ui <- fluidPage(
  titlePanel("Trends, Patterns & Anomalies in Academic Performance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a Variable to Plot:", 
                  choices = c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400"),
                  selected = "CGPA"),
      checkboxInput("log_scale", "Use Log Scale", FALSE),
      checkboxInput("show_line", "Show Trend Line", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📈 Trend by Graduation Year", plotlyOutput("linePlot")),
        tabPanel("📊 Boxplots by Year", plotlyOutput("boxPlot")),
        tabPanel("🔥 Correlation Heatmap", plotOutput("corrPlot")),
        tabPanel("⚡ Anomaly Detection (Scatter)", plotlyOutput("scatterPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$linePlot <- renderPlotly({
    summary_data <- data %>%
      group_by(YoG) %>%
      summarise(avg = mean(get(input$variable), na.rm = TRUE))
    
    p <- ggplot(summary_data, aes(x = YoG, y = avg)) +
      geom_line(color = "blue") +
      geom_point(color = "darkblue", size = 3) +
      labs(title = paste("Average", input$variable, "by Year of Graduation"), x = "YoG", y = input$variable)
    
    if (input$show_line) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "red")
    }
    
    if (input$log_scale) {
      p <- p + scale_y_log10()
    }
    
    ggplotly(p)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(data, aes(x = factor(YoG), y = get(input$variable), fill = factor(YoG))) +
      geom_boxplot() +
      labs(title = paste("Distribution of", input$variable, "by Graduation Year"), x = "YoG", y = input$variable) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$corrPlot <- renderPlot({
    subdata <- data %>% 
      select(SGPA, CGPA, CGPA100, CGPA200, CGPA300, CGPA400) %>%
      cor(use = "complete.obs")
    
    corrplot(subdata, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.8)
  })
  
  output$scatterPlot <- renderPlotly({
    plot_ly(data, x = ~SGPA, y = ~get(input$variable),
            type = 'scatter', mode = 'markers',
            marker = list(color = ~CGPA, colorscale = "Viridis", showscale = TRUE)) %>%
      layout(title = paste("Anomaly Detection: SGPA vs", input$variable),
             xaxis = list(title = "SGPA"),
             yaxis = list(title = input$variable))
  })
}

shinyApp(ui = ui, server = server)
