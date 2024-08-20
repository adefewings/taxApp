library(shiny)
library(plotly)

text_data <- read.csv("translations.csv", stringsAsFactors = FALSE)

# Define the function to convert the dataframe to the nested list
text_resources_func <- function(df) {
  # Ensure 'key', 'english', and 'welsh' columns exist
  if (!all(c("key", "english", "welsh") %in% names(df))) {
    stop("CSV file must contain 'key', 'english', and 'welsh' columns.")
  }
  
  # Extract unique keys
  keys <- df$key
  
  # Create lists for each language
  english_list <- setNames(as.list(df$english), keys)
  welsh_list <- setNames(as.list(df$welsh), keys)
  
  # Combine into a named list
  list(
    English = english_list,
    Welsh = welsh_list
  )
}

# Apply the function to create the 'text_resources' list
text_resources <- text_resources_func(text_data)

ui <- fluidPage(
  # Title and button layout
  fluidRow(
    column(10, 
           titlePanel(textOutput("title"))
    ),
    column(2, 
           align = "right",
           actionButton("translateButton", textOutput("translate_button"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", textOutput("dataset_label"), 
                  choices = c(textOutput("dataset1"), textOutput("dataset2")), 
                  selected = textOutput("dataset1")),
      actionButton("toggleButton", textOutput("divide_button")),
      actionButton("viewButton", textOutput("show_sum_button"))
    ),
    
    mainPanel(
      plotlyOutput("stackedPlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to keep track of button states and language
  values <- reactiveValues(
    divide = FALSE,
    show_sum = FALSE,
    language = "English"
  )
  
  # Update button labels based on the current state and language
  updateButtonLabels <- function() {
    updateActionButton(session, "toggleButton", label = ifelse(values$divide, 
                                                               text_resources[[values$language]]$revert, 
                                                               text_resources[[values$language]]$divide_by_people))
    updateActionButton(session, "translateButton", label = text_resources[[values$language]]$translate_button)
  }
  
  # Observe the "Translate" button to switch language
  observeEvent(input$translateButton, {
    values$language <- ifelse(values$language == "English", "Welsh", "English")
  updateButtonLabels()  # Update button labels immediately
  })

# Observe the "Toggle" button to toggle state
observeEvent(input$toggleButton, {
  values$divide <- !values$divide
  updateButtonLabels()  # Update button label when toggled
})

observeEvent(input$viewButton, {
  values$show_sum <- !values$show_sum
})

# Reactive expression to return data based on selected dataset and button states
data_reactive <- reactive({
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
  
  if (values$show_sum) {
    list(
      stacked = vector1 + vector2 + vector3,
      labels = labels
    )
  } else {
    list(
      stacked = rbind(vector1, vector2, vector3),
      labels = labels
    )
  }
})

# Dynamic text output based on selected language
output$title <- renderText({
  text_resources[[values$language]]$title
})

output$dataset_label <- renderText({
  text_resources[[values$language]]$dataset
})

output$divide_button <- renderText({
  text_resources[[values$language]]$divide_button
})

output$show_sum_button <- renderText({
  text_resources[[values$language]]$show_sum_button
})

output$translate_button <- renderText({
  text_resources[[values$language]]$translate_button
})
output$dataset1 <- renderText({
  text_resources[[values$language]]$dataset1
})
output$dataset2 <- renderText({
  text_resources[[values$language]]$dataset2
})

# Generate Stacked Bar Chart or Sum Chart using Plotly
output$stackedPlot <- renderPlotly({
  data <- data_reactive()
  
  x_categories <- factor(data$labels, levels = data$labels)
  
  if (values$show_sum) {
    plot_ly(
      x = x_categories,
      y = ~data$stacked,
      type = 'bar',
      name = text_resources[[values$language]]$sum_plot_title,
      marker = list(color = 'rgba(255, 99, 132, 0.6)')
    ) %>%
      layout(
        barmode = 'group',
        title = text_resources[[values$language]]$sum_plot_title,
        xaxis = list(title = text_resources[[values$language]]$xaxis_title),
        yaxis = list(title = text_resources[[values$language]]$yaxis_title_sum)
      )
    
  } else {
    plot_ly(
      x = x_categories,
      y = ~data$stacked[1,],
      type = 'bar',
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
        title = text_resources[[values$language]]$stacked_plot_title,
        xaxis = list(title = text_resources[[values$language]]$xaxis_title),
        yaxis = list(title = text_resources[[values$language]]$yaxis_title_stacked)
      )
  }
})
}

shinyApp(ui, server)
