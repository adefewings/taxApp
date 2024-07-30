library(shiny)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name:", value = ""),
      
      sliderInput("rating", "rate your experiance (1-5):",
                  min = 1, max = 5, value = 3, step = 1)
    ),
    
    mainPanel(
      textOutput("greeting"),
      
      textOutput("selected_rating")
    )
  )
)


server <- function(input, output, sesssion) {
  output$greeting <- renderText({
    if (input$name != "") {
      paste("Hello", input$name, "! Welcome to Shiny")
    } else {
      "Enter your name above."
    }
  })
  
  output$selected_rating <- renderText({
    paste("You rated your experiance as:", input$rating)
  })
}

shinyApp(ui = ui, server = server)