library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Reactive Stacked Bar Chart"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:", 
                  choices = c("Dataset 1", "Dataset 2"), 
                  selected = "Dataset 1"),
      actionButton("toggleButton", "Divide by 10")
    ),
    
    mainPanel(
      plotlyOutput("stackedPlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to keep track of button state
  values <- reactiveValues(divide = FALSE)
  
  observeEvent(input$toggleButton, {
    values$divide <- !values$divide
    if (values$divide) {
      updateActionButton(session, "toggleButton", label = "Revert")
    } else {
      updateActionButton(session, "toggleButton", label = "Divide by 10")
    }
  })
  
  # Reactive expression to return data based on selected dataset and button state
  data_reactive <- reactive({
    # Define constants
    divisor <- if (values$divide) 10 else 1
    
    if (input$dataset == "Dataset 1") {
      vector1 <- c(5, 8, 12, 6, 15, 10, 8, 13, 7, 9, 14, 11, 10) / divisor
      vector2 <- c(3, 4, 6, 2, 5, 3, 4, 6, 3, 5, 7, 4, 3) / divisor
      vector3 <- c(2, 3, 4, 2, 3, 2, 3, 4, 2, 2, 3, 2, 2) / divisor
    } else if (input$dataset == "Dataset 2") {
      vector1 <- c(7, 11, 16, 8, 18, 12, 10, 15, 9, 12, 17, 13, 12) / divisor
      vector2 <- c(4, 5, 7, 3, 6, 4, 5, 7, 4, 6, 8, 5, 4) / divisor
      vector3 <- c(3, 4, 5, 3, 4, 3, 4, 5, 3, 3, 4, 3, 3) / divisor
    }
    labels <- c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100", "110", "120", "120+")
    
    list(
      stacked = rbind(vector1, vector2, vector3),
      labels = labels
    )
  })
  
  # Generate Stacked Bar Chart using Plotly
  output$stackedPlot <- renderPlotly({
    data <- data_reactive()
    
    # Ensure x-axis categories are factors with the correct order
    x_categories <- factor(data$labels, levels = data$labels)
    
    plot_ly(
      type = 'bar',
      x = x_categories,
      y = ~data$stacked[1,],
      name = 'Vector 1',
      marker = list(color = 'rgba(255, 99, 132, 0.6)')
    ) %>%
      add_trace(
        y = ~data$stacked[2,],
        name = 'Vector 2',
        marker = list(color = 'rgba(54, 162, 235, 0.6)')
      ) %>%
      add_trace(
        y = ~data$stacked[3,],
        name = 'Vector 3',
        marker = list(color = 'rgba(75, 192, 192, 0.6)')
      ) %>%
      layout(
        barmode = 'stack',
        title = "Stacked Bar Chart with Plotly",
        xaxis = list(title = "Categories"),
        yaxis = list(title = "Values")
      )
  })
}

shinyApp(ui, server)
