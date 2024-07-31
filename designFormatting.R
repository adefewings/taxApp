server <- function(input, output, session) {
  
  # Reactive expression to generate fruit data based on tax choice
  reactive_fruit_data <- reactive({
    req(input$tax_choice)  # Ensure tax_choice is available
    
    # Determine fruit counts based on tax choice
    if (input$tax_choice == "current") {
      data.frame(
        fruit = c("apple", "orange", "pear"),
        count = c(10, 20, 30)
      )
    } else if (input$tax_choice == "scottish" || input$tax_choice == "fully devolved") {
      data.frame(
        fruit = c("apple", "orange", "pear"),
        count = c(20, 20, 20)
      )
    } else {
      # Default data or handle other cases
      data.frame(
        fruit = c("apple", "orange", "pear"),
        count = c(0, 0, 0)  # Example default case
      )
    }
  })
  
  # Observe tax choice changes and update pie chart
  output$pieChart <- renderPlotly({
    fruit_data <- reactive_fruit_data()
    
    plot_ly(fruit_data, labels = ~fruit, values = ~count, type = 'pie') %>%
      layout(
        title = 'Fruit Distribution',
        margin = list(l = 20, r = 20, b = 20, t = 30),  # Adjust margins
        paper_bgcolor = 'lightgray',  # Background color of the plot area
        plot_bgcolor = 'white'  # Background color of the chart area
      )
  })
  
  # Other existing code (e.g., for tax calculations and other observers)...
}
