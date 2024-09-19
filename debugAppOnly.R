library(shiny)
library(DT)

ui <- fluidPage(
  DTOutput("my_table"),
  
  tags$head(
    tags$style(HTML("
      #my_table tbody tr:first-child td:first-child {
        background-color: yellow !important;
      }
    "))
  )
)

server <- function(input, output, session) {
  output$my_table <- renderDT({
    datatable(
      data.frame(
        A = c("R1C1", "R2C1", "R3C1"),
        B = c("R1C2", "R2C2", "R3C2")
      ),
      options = list(dom = 't')  # hide search and other options
    )
  })
}

shinyApp(ui, server)