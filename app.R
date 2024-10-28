# Save this as app.R

library(shiny)
library(forecast)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("ARIMA Time Series Model"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Choose Data Source"),
      radioButtons("data_source", "Select Data Source:",
                   choices = c("AirPassengers Dataset", "Upload CSV File")),
      conditionalPanel(
        condition = "input.data_source == 'Upload CSV File'",
        fileInput("file", "Upload Time Series CSV File:",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      numericInput("p", "AR Order (p):", value = 1, min = 0, max = 5),
      numericInput("d", "Differencing Order (d):", value = 1, min = 0, max = 2),
      numericInput("q", "MA Order (q):", value = 1, min = 0, max = 5),
      actionButton("fit", "Fit ARIMA Model")
    ),
    
    mainPanel(
      plotOutput("tsPlot"),
      plotOutput("forecastPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to read the uploaded file
  ts_data <- reactive({
    if (input$data_source == "Upload CSV File") {
      req(input$file)  # Ensure a file is uploaded
      data <- read.csv(input$file$datapath)
      # Assuming the first column is the time series data
      ts(data[[1]], frequency = 12)  # Adjust frequency if needed
    } else {
      # Use AirPassengers dataset if not uploading a file
      AirPassengers
    }
  })
  
  model_fit <- eventReactive(input$fit, {
    req(ts_data())  # Ensure time series data is available
    arima(ts_data(), order = c(input$p, input$d, input$q))
  })
  
  output$tsPlot <- renderPlot({
    req(ts_data())  # Ensure time series data is available
    autoplot(ts_data()) + 
      ggtitle("Time Series Data") +
      xlab("Time") +
      ylab("Value") +
      theme_minimal()
  })
  
  output$forecastPlot <- renderPlot({
    req(model_fit())  # Ensure the model is fitted
    forecasted_values <- forecast(model_fit())
    autoplot(forecasted_values) + 
      ggtitle("ARIMA Model Forecast") +
      xlab("Time") +
      ylab("Forecasted Value") +
      theme_minimal()
  })
  
  output$summary <- renderPrint({
    req(model_fit())  # Ensure the model is fitted
    summary(model_fit())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
