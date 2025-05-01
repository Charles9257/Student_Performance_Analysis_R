


install.packages(c("shiny", "plotly", "dplyr","moments","ggplot2"))


library(shiny)
library(plotly)
library(dplyr)
library(moments)
library(ggplot2)

# Load and clean data
data <- read.csv("sampled_data.csv")
colnames(data) <- make.names(colnames(data))  # make variable names safe

# UI
ui <- fluidPage(
  titlePanel("Interactive Academic Performance Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select a Variable:",
                  choices = c("CGPA", "SGPA", "CGPA100", "CGPA200", "CGPA300", "CGPA400"),
                  selected = "CGPA"),
      sliderInput("bins", "Histogram Bins:", min = 5, max = 50, value = 20),
      hr(),
      verbatimTextOutput("stats"),
      verbatimTextOutput("correlation"),
      hr(),
      downloadButton("downloadHist", "Download Histogram"),
      downloadButton("downloadBox", "Download Boxplot"),
      downloadButton("downloadScatter", "Download Scatter Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotlyOutput("histogram")),
        tabPanel("Boxplot", plotlyOutput("boxplot")),
        tabPanel("Scatter Plot (vs SGPA)", plotlyOutput("scatter"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  selected_data <- reactive({
    na.omit(data[[input$variable]])
  })
  
  output$stats <- renderPrint({
    x <- selected_data()
    cat("Descriptive Statistics for", input$variable, "\n")
    cat("Skewness :", round(skewness(x), 3), "\n")
    cat("Kurtosis :", round(kurtosis(x), 3), "\n")
  })
  
  output$correlation <- renderPrint({
    x <- na.omit(data[[input$variable]])
    y <- na.omit(data$SGPA)
    common_rows <- complete.cases(data[, c("SGPA", input$variable)])
    r <- cor(data$SGPA[common_rows], data[[input$variable]][common_rows], method = "pearson")
    cat("Pearson Correlation (SGPA vs", input$variable, "):", round(r, 3), "\n")
  })
  
  output$histogram <- renderPlotly({
    plot_ly(x = selected_data(), type = "histogram", nbinsx = input$bins) %>%
      layout(
        title = paste("Histogram of", input$variable),
        xaxis = list(title = input$variable),
        yaxis = list(title = "Count")
      )
  })
  
  output$boxplot <- renderPlotly({
    plot_ly(y = selected_data(), type = "box", name = input$variable) %>%
      layout(
        title = paste("Boxplot of", input$variable),
        yaxis = list(title = input$variable)
      )
  })
  
  output$scatter <- renderPlotly({
    plot_ly(data = data, x = ~SGPA, y = ~get(input$variable), 
            type = 'scatter', mode = 'markers',
            marker = list(size = 7, opacity = 0.6)) %>%
      layout(
        title = paste(input$variable, "vs SGPA"),
        xaxis = list(title = "SGPA"),
        yaxis = list(title = input$variable)
      )
  })
  
  # Download handlers (using ggplot2 for saving as image)
  output$downloadHist <- downloadHandler(
    filename = function() { paste0("histogram_", input$variable, ".png") },
    content = function(file) {
      g <- ggplot(data, aes_string(x = input$variable)) +
        geom_histogram(bins = input$bins, fill = "skyblue", color = "black") +
        ggtitle(paste("Histogram of", input$variable)) +
        theme_minimal()
      ggsave(file, plot = g, width = 7, height = 5)
    }
  )
  
  output$downloadBox <- downloadHandler(
    filename = function() { paste0("boxplot_", input$variable, ".png") },
    content = function(file) {
      g <- ggplot(data, aes_string(y = input$variable)) +
        geom_boxplot(fill = "orange") +
        ggtitle(paste("Boxplot of", input$variable)) +
        theme_minimal()
      ggsave(file, plot = g, width = 5, height = 5)
    }
  )
  
  output$downloadScatter <- downloadHandler(
    filename = function() { paste0("scatter_", input$variable, "_vs_SGPA.png") },
    content = function(file) {
      g <- ggplot(data, aes_string(x = "SGPA", y = input$variable)) +
        geom_point(color = "steelblue", alpha = 0.6) +
        ggtitle(paste("Scatter Plot of", input$variable, "vs SGPA")) +
        theme_minimal()
      ggsave(file, plot = g, width = 7, height = 5)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
