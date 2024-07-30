library(shiny)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #FFD700;
        }
        .title {
          color: #333;
        }
      ")
    )
  ),
  
  tags$head(
    tags$style(HTML("
      .grey-out {
        pointer-events: none;
        background-color: #f0f0f0 !important; /* Grey background color */
        color: #999999; /* Grey text color */
      }
    "))
  ),
  
  titlePanel("Income Tax Calculator 2022/23"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tax_choice", "Select Tax System:",
                  choices = c("current", "scottish", "fully devolved"),
                  selected = "current"),
      tabsetPanel(
        tabPanel("incomeTax",
                 #sliderInput("PA", "Personal Allowance (PA):", min = 0, max = 20000, value = 12500),
                 numericInput("PA", "Personal Allowance (PA):", 12500),
                 sliderInput("PAlimit", "Personal Allowance Limit:", min = 0, max = 200000, value = 100000),
                 numericInput("SRthreshold", "Starter Rate Threshold:", 14500),
                 #sliderInput("BRthreshold", "Basic Rate Threshold:", min = 0, max = 50000, value = 26500),
                 numericInput("BRthreshold", "Basic Rate Threshold:", 26500),
                 numericInput("IRthreshold", "Intermediate Rate Threshold:", 43500),
                 numericInput("HRthreshold", "Higher Rate Threshold:", 125000),
                 #sliderInput("HRthreshold", "Higher Rate Threshold:", min = 0, max = 200000, value = 125000),
                 #numericInput("TRthreshold", "Top Rate Threshold:", 14500),
                 sliderInput("SR", "Starter Rate:", min = 0, max = 1, value = 0.19, step = 0.01),
                 sliderInput("BR", "Basic Rate:", min = 0, max = 1, value = 0.2, step = 0.01),
                 sliderInput("IR", "Intermediate Rate:", min = 0, max = 1, value = 0.21, step = 0.01),
                 sliderInput("HR", "Higher Rate:", min = 0, max = 1, value = 0.41, step = 0.01),
                 sliderInput("AR", "Additional Rate:", min = 0, max = 1, value = 0.46, step = 0.01)
        )
      )
    ),
    mainPanel(
      tags$img(src = "bangorLogo.png", height = "150px", width = "50%", style = "display: block; margin-left: auto; margin-right: auto;"),
      verbatimTextOutput("totalTaxOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  
  observe({
    enabled_sliders <- character(0)
    disabled_sliders <- character(0)
    
    if ((input$tax_choice == "current") || (input$tax_choice == "fully devolved")) {
      enabled_sliders <- c("BR", "HR", "AR")
      disabled_sliders <- c("SR", "IR")
      runjs("$('#SRthreshold').addClass('grey-out').prop('disabled', true);")
      runjs("$('#IRthreshold').addClass('grey-out').prop('disabled', true);")
    } else if (input$tax_choice == "scotland") {
      enabled_sliders <- c("SR","BR", "IR","HR", "AR")
      disabled_sliders <- character(0)
      runjs("$('#SRthreshold').removeClass('grey-out').prop('disabled', false);")
      runjs("$('#IRthreshold').removeClass('grey-out').prop('disabled', fasle);")
    }
    
    #else if (input$tax_choice == "fully devolved") {
    #  enabled_sliders <- character(0)
    #  disabled_sliders <- c("basic_rate", "starter_rate", "intermediate_rate", "higher_rate", "additional_rate")
    #  runjs("$('#additional_info').addClass('grey-out').prop('disabled', true);")
    #}
    
    # Enable sliders
    lapply(enabled_sliders, function(slider_id) {
      runjs(sprintf("$('#%s').parent().removeClass('grey-out'); $('#%s').prop('disabled', false);", slider_id, slider_id))
    })
    
    # Disable sliders
    lapply(disabled_sliders, function(slider_id) {
      runjs(sprintf("$('#%s').parent().addClass('grey-out'); $('#%s').prop('disabled', true);", slider_id, slider_id))
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
