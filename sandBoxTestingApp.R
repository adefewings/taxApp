library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #CCCCCC;
        }
        .title {
          color: #333;
        }
        .responsive-img {
          max-width: 100%;
          height: auto;
        }
        .grey-out {
        pointer-events: none;
        background-color: #f0f0f0 !important; /* Grey background color */
        color: #999999; /* Grey text color */
      }
      .grey-out .irs {
        pointer-events: none !important;
        opacity: 0.6;
      }
      .grey-out .irs-line {
        background: #777777 !important;
      }
      .grey-out .irs-bar {
        background: #777777 !important;
      }
      .grey-out .irs-handle {
        background: #777777 !important;
      }
      .fixed-size-img {
        height: 150px; /* Adjust height as needed */
        width: auto; /* Width will scale proportionally */
        max-width: 100%; /* Ensure it doesn't exceed container width */
        display: block;
        margin-left: auto;
        margin-right: auto;
        }
      ")
    )
  ),
  

  
  titlePanel("Income Tax Calculator 2022/23"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tax_choice", "Select Tax System:",
                  choices = c("current", "scottish", "fully devolved"),
                  selected = "current"),
      tabsetPanel(
        tabPanel("incomeTax",
                 numericInput("PA", "Personal Allowance (PA):", 12500),
                 sliderInput("PAlimit", "Personal Allowance Limit:", min = 0, max = 200000, value = 100000),
                 numericInput("SRthreshold", "Starter Rate Threshold:", 14500),
                 numericInput("BRthreshold", "Basic Rate Threshold:", 26500),
                 numericInput("IRthreshold", "Intermediate Rate Threshold:", 43500),
                 numericInput("HRthreshold", "Higher Rate Threshold:", 125000),
                 sliderInput("SR", "Starter Rate:", min = 0, max = 1, value = 0.19, step = 0.01),
                 sliderInput("BR", "Basic Rate:", min = 0, max = 1, value = 0.2, step = 0.01),
                 sliderInput("IR", "Intermediate Rate:", min = 0, max = 1, value = 0.21, step = 0.01),
                 sliderInput("HR", "Higher Rate:", min = 0, max = 1, value = 0.41, step = 0.01),
                 sliderInput("AR", "Additional Rate:", min = 0, max = 1, value = 0.46, step = 0.01)
        )
      )
    ),
    mainPanel(
      #tags$img(src = "bangorLogo.png", height = "150px", width = "50%", style = "display: block; margin-left: auto; margin-right: auto;"),
      tags$img(src = "bangorLogo.png", class = "fixed-size-img"),
      verbatimTextOutput("totalTaxOutput")
    )
  )
)


server <- function(input, output) {
  observe({
    enabled_ids <- character(0)
    disabled_ids <- character(0)
    
    if (input$tax_choice == "current" || input$tax_choice == "fully devolved") {
      enabled_ids <- c("BR", "HR", "AR")
      disabled_ids <- c("SR", "IR", "SRthreshold", "IRthreshold")
    } else if (input$tax_choice == "scottish") {
      enabled_ids <- c("SR", "BR", "IR", "HR", "AR", "SRthreshold", "IRthreshold")
      disabled_ids <- character(0)
    }
    
    # Enable sliders
    lapply(enabled_ids, function(id) {
      runjs(sprintf("$('#%s').prop('disabled', false).parent().removeClass('grey-out');", id))
    })
    
    # Disable sliders
    lapply(disabled_ids, function(id) {
      runjs(sprintf("$('#%s').prop('disabled', true).parent().addClass('grey-out');", id))
    })
  })
  
  output$totalTaxOutput <- renderPrint({
    # Define Parameters from inputs
    PA <- input$PA
    PAlimit <- input$PAlimit
    BRthreshold <- input$BRthreshold
    HRthreshold <- input$HRthreshold
    BR <- input$BR
    HR <- input$HR
    AR <- input$AR
    
    # Import taxable income distribution
    TIDist <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")
    
    # Calculate Income by tax bracket
    TIDist$PA <- NA
    TIDist$PA[TIDist$TaxableIncome <= PAlimit] <- PA
    TIDist$PA[TIDist$TaxableIncome > PAlimit] <- pmax(0, PA - 0.5 * (TIDist$TaxableIncome[TIDist$TaxableIncome > PAlimit] - PAlimit))
    TIDist$PAincome <- pmin(TIDist$TaxableIncome, TIDist$PA)
    TIDist$BRincome <- NA
    TIDist$BRincome[TIDist$PAincome < TIDist$PA] <- 0
    TIDist$BRincome[TIDist$PAincome >= TIDist$PA] <- pmin(TIDist$TaxableIncome[TIDist$PAincome >= TIDist$PA] - TIDist$PA[TIDist$PAincome >= TIDist$PA], BRthreshold)
    TIDist$HRincome <- NA
    TIDist$HRincome[TIDist$BRincome < BRthreshold] <- 0
    TIDist$HRincome[TIDist$BRincome == BRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$BRincome == BRthreshold] - TIDist$PA[TIDist$BRincome == BRthreshold] - BRthreshold, HRthreshold - BRthreshold)
    TIDist$ARincome <- NA
    TIDist$ARincome[TIDist$HRincome < HRthreshold - BRthreshold] <- 0
    TIDist$ARincome[TIDist$HRincome == HRthreshold - BRthreshold] <- TIDist$TaxableIncome[TIDist$HRincome == HRthreshold - BRthreshold] - TIDist$PA[TIDist$HRincome == HRthreshold - BRthreshold] - HRthreshold
    
    # Calculate tax payable by tax bracket
    TIDist$BRtax <- BR * TIDist$BRincome
    TIDist$HRtax <- HR * TIDist$HRincome
    TIDist$ARtax <- AR * TIDist$ARincome
    
    # Calculate total income tax payable
    TIDist$TotalTax <- (TIDist$BRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
    total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
    paste("Total tax return from income tax = ", total_tax_sum)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
