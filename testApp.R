# Load the Shiny package
library(shiny)
library(datasets)  # Load datasets package for sample datasets

# Define the UI
ui <- fluidPage(
  titlePanel("Enhanced Shiny App"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Controls",
              selectInput("dataset", "Choose a dataset:",
                          choices = c("faithful", "cars", "pressure")),
              sliderInput("bins",
                          "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30),
              checkboxInput("show_density", "Show density curve", FALSE),
              numericInput("obs", "Number of observations to view:", 10)
    ),
    tabPanel("textBox",
             textInput("text_input", "enter some text:", "Hello, shiny!")
             ),
    tabPanel("number_input",
             textInput("number_input", "please entera number:")
             ),
    tabPanel("click here",
             sliderInput("fav", "races:",
                         min =1,
                         max = 100,
                         value = 20),
             )

      )
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summary"),
      tableOutput("view"),
      verbatimTextOutput("text_output"),
      verbatimTextOutput("number_output")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to fetch the selected dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "faithful" = faithful,
           "cars" = cars,
           "pressure" = pressure)
  })
  
  output$distPlot <- renderPlot({
    data <- datasetInput()[, 1]  # Use the first column of the selected dataset
    bins <- seq(min(data), max(data), length.out = input$bins + 1)
    
    hist(data, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Value', main = paste('Histogram of', input$dataset))
    
    if (input$show_density) {
      dens <- density(data)
      lines(dens, col = 'blue')
    }
  })
  
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
  output$text_output <- renderText({
    paste("enteredText", input$text_input)
  })
  
  
  output$number_output <- renderText({
    input_val <- input$number_input
    if (is.na(as.numeric(input_val))){
      "please enter a number, not letters!!!!!"
    }else {
      paste("Result: ", 3.14 * as.numeric(input_val))
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
