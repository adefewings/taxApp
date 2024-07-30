library(shiny)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #CCCCCC;
        }
        .title {
          color: #333;
        }
        /* Custom CSS for greyed-out inputs and sliders */
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
        .input-group {
          display: flex;
          align-items: center;
          margin-bottom: 10px; /* Space between input groups */
        }
        .input-group label {
          width: 200px; /* Fixed width for labels */
          margin-right: 10px; /* Space between label and input */
        }
        .input-group .shiny-input-container {
          flex-grow: 1; /* Let inputs take the remaining space */
        }
        .output-container {
          background-color: #f0f0f0;
          padding: 10px;
          border-radius: 5px;
          margin-top: 10px;
          margin-left: 100px;
        }
        .calculate-button-container{
          background-color: #f0f0f0;
          padding: 10px;
          border-radius: 3px;
          margin-top: 10px;
          margin-left: 100px;
          marin-right:100px;
          display: inline-block; 
          padding-left: 20px; 
          padding-right: 20px;
        }
      ")
    )
  ),
  
  titlePanel("Income Tax Calculator 2022/23"),
  
  # Dropdown menu above the tabs
  
  
  # Tabs for different content
  tabsetPanel(
    id = "tabs",
    tabPanel("Tax calcs", value = "tab1", 
             fluidRow(
               selectInput("tax_choice", "Select Tax System:",
                           choices = c("current", "scottish", "fully devolved"),
                           selected = "current"),
               column(6,
                      tabPanel("incomeTax",
                               div(class = "input-group",
                                   tags$label("Personal Allowance:", `for` = "PA"),
                                   numericInput("PA", NULL, 12500)),
                               div(class = "input-group",
                                   tags$label("Personal Allowance Limit:", `for` = "PAlimit"),
                                   sliderInput("PAlimit", NULL, min = 0, max = 200000, value = 100000)),
                               div(class = "input-group",
                                   tags$label("Starter Rate Threshold:", `for` = "SRthreshold"),
                                   numericInput("SRthreshold", NULL, 14500)),
                               div(class = "input-group",
                                   tags$label("Basic Rate Threshold:", `for` = "BRthreshold"),
                                   numericInput("BRthreshold", NULL, 26500)),
                               div(class = "input-group",
                                   tags$label("Intermediate Rate Threshold:", `for` = "IRthreshold"),
                                   numericInput("IRthreshold", NULL, 43500)),
                               div(class = "input-group",
                                   tags$label("Higher Rate Threshold:", `for` = "HRthreshold"),
                                   numericInput("HRthreshold", NULL, 125000)),
                               div(class = "input-group",
                                   tags$label("Starter Rate:", `for` = "SR"),
                                   sliderInput("SR", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               div(class = "input-group",
                                   tags$label("Basic Rate:", `for` = "BR"),
                                   sliderInput("BR", NULL, min = 0, max = 1, value = 0.2, step = 0.01)),
                               div(class = "input-group",
                                   tags$label("Intermediate Rate:", `for` = "IR"),
                                   sliderInput("IR", NULL, min = 0, max = 1, value = 0.21, step = 0.01)),
                               div(class = "input-group",
                                   tags$label("Higher Rate:", `for` = "HR"),
                                   sliderInput("HR", NULL, min = 0, max = 1, value = 0.41, step = 0.01)),
                               div(class = "input-group",
                                   tags$label("Additional Rate:", `for` = "AR"),
                                   sliderInput("AR", NULL, min = 0, max = 1, value = 0.46, step = 0.01))
                      )
               ),
               column(5,
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(class = "output-container",
                          verbatimTextOutput("totalTaxOutput")
                      ),
                      div(class = "calculate-button-container",
                          actionButton("calculate", label = "calculate with new filters")
                      ),
            
                      div(class = "output-container",
                          verbatimTextOutput("newTotalTaxOutput")
                      )
               )
             )
    )
  ),
  #land transactional Tax:
  tabPanel("Land Transactional Tax", value = "land tax", h4("LTT is a tax on property transactions in Wales only. Replaced Stamp Duty Land Tax (SDLT) which is still in place in England"),
           div(style = "height: 20px;", p("")), # Empty placeholder
           div(style = "ndr-selection",
               selectInput("tax_choice", "Select Income Tax System:",
                           choices = c("Wales", "Scotland", "England"),
                           selected = "Wales")),
           div(style = "height: 20px;", p("")), # Empty placeholder
           fluidRow(
             column(6,
                    tabPanel("NDR",
                             div(class = "input-group-ndr",
                                 tags$label(id = "rateableValue-label","Proposed change for rateable values:", `for` = "rateableValueChange"),
                                 tags$p("Band A:"),
                                 numericInput("rateableValueChange", NULL, 0)),
                             div(class = "input-group-ndr",
                                 tags$label(id="stdMultiplier-label","Standard multiplier value:", `for` = "stdMult"),
                                 numericInput("stdMult", NULL, 0)),
                             div(class = "input-group-ndr",
                                 tags$label(id="smallBusinessMultiplier-label","Small Business multiplier:", `for` = "smallBusMult"),
                                 numericInput("smallBusMult", NULL, 0)),
                             
                    )
             )
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
      runjs("$('#IRthreshold').removeClass('grey-out').prop('disabled', false);")
    }
    
    lapply(enabled_sliders, function(slider_id) {
      runjs(sprintf("$('#%s').parent().removeClass('grey-out'); $('#%s').prop('disabled', false);", slider_id, slider_id))
    })
    
    lapply(disabled_sliders, function(slider_id) {
      runjs(sprintf("$('#%s').parent().addClass('grey-out'); $('#%s').prop('disabled', true);", slider_id, slider_id))
    })
  })
  
  output$totalTaxOutput <- renderText({
    PA <- input$PA
    PAlimit <- input$PAlimit
    BRthreshold <- input$BRthreshold
    HRthreshold <- input$HRthreshold
    BR <- input$BR
    HR <- input$HR
    AR <- input$AR
    
    TIDist <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")
    
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
    
    TIDist$BRtax <- BR * TIDist$BRincome
    TIDist$HRtax <- HR * TIDist$HRincome
    TIDist$ARtax <- AR * TIDist$ARincome
    
    TIDist$TotalTax <- (TIDist$BRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
    total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
    paste("Total tax return from income tax =", total_tax_sum)
  })
  
  
  output$newTotalTaxOutput <- renderText({
    newAmount <- 301
    paste("new total tax amount=", newAmount)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
