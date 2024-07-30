library(shiny)

ui <- fluidPage(
  # Custom CSS to increase font size
  tags$style(HTML("
        /* Increase font size of radio button labels */
        .radio-inline {
            font-size: 20px ;
        }
        label {
            font-size: 20px ;
        }
    ")),
  
  # Radio button input
  radioButtons("buttonChoice", 
               "Please select a fruit", 
               choices = c("apple", "orange", "pear"), 
               selected = "apple", 
               inline = FALSE)
)

server <- function(input, output) {
  # Server logic here
}

shinyApp(ui = ui, server = server)
