library(shiny)
library(shinyjs)

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
        /* Custom CSS for greyed-out inputs and sliders */
        .grey-out {
          pointer-events: none;
          background-color: # !important; /* Grey background color */
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
          margin-bottom: 2px; /* Space between input groups */
        }
        .input-group label {
          width: 200px; /* Fixed width for labels */
          margin-right: 5px; /* Space between label and input */
        }
        .input-group .shiny-input-container {
          flex-grow: 0.2; /* Let inputs take the remaining space */
        }
        .output-box {
          position: absolute;
          right: 100px;
          top:800px; /* Adjust top position as needed */
          width: 30%;
          background-color: #FFFFFF;
          padding: 10px;
          box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
          border-radius: 5px;
        }
                .output-container {
          background-color: #f0f0f0;
          padding: 10px;
          border-radius: 5px;
          margin-top: 10px;
        }
      ")
    )
  ),
  
  titlePanel("Income Tax Calculator 2022/23"),
  
  # Dropdown menu above the tabs
  selectInput("tax_choice", "Select Tax System:",
              choices = c("current", "scottish", "fully devolved"),
              selected = "current"),
  
  # Tabs for different content
  tabsetPanel(
    id = "tabs",
    tabPanel("Tax calcs", value = "tab1", 
             
             tabPanel("incomeTax",
                      div(class = "input-group",
                          tags$label("Personal Allowance (PA):", `for` = "PA"),
                          numericInput("PA", NULL, 12500)),
                      div(class = "input-group",
                          tags$label("Personal Allowance Limit:", `for` = "PAlimit"),
                          sliderInput("PAlimit", NULL, min = 0, max = 200000, value = 100000)),
                      div(class = "input-group",
                          tags$label("Starter Rate Threshold:", `for` = "SRthreshold"),
                          numericInput("SRthreshold", NULL, 2000)),
                      div(class = "input-group",
                          tags$label("Basic Rate Threshold:", `for` = "BRthreshold"),
                          numericInput("BRthreshold", NULL, 11000)),
                      div(class = "input-group",
                          tags$label("Intermediate Rate Threshold:", `for` = "IRthreshold"),
                          numericInput("IRthreshold", NULL, 8000)),
                      div(class = "input-group",
                          tags$label("Higher Rate Threshold:", `for` = "HRthreshold"),
                          numericInput("HRthreshold", NULL, 107000)),
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
                          sliderInput("AR", NULL, min = 0, max = 1, value = 0.46, step = 0.01)),
                      div(class = "output-box",
                          #tags$label("Total tax from income tax", `for` = "totalTaxOutput"),
                          verbatimTextOutput("totalTaxOutput"))
             )
             
    ),
    tabPanel("More Info", value = "more info", h4("More info"))
  )
)


server <- function(input, output, session) {
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
  
  output$totalTaxOutput <- renderText({
    # Define Parameters from inputs
    PA <- input$PA
    PAlimit <- input$PAlimit
    SRthreshold <- input$SRthreshold
    BRthreshold <- input$BRthreshold
    IRthreshold <- input$IRthreshold
    HRthreshold <- input$HRthreshold
    HRthreshold <- input$HRthreshold
    SR <- input$SR
    BR <- input$BR
    IR <- input$IR
    HR <- input$HR
    AR <- input$AR
    
    
 
    
    # Import taxable income distribution
    TIDist <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")
    
    # Calculate Income by tax bracket
    
    if (input$tax_choice == "scottish"){
      TIDist$PA <- NA
      TIDist$PA[TIDist$TaxableIncome <= PAlimit] <- PA
      TIDist$PA[TIDist$TaxableIncome > PAlimit] <- pmax(0, PA - 0.5 * (TIDist$TaxableIncome[TIDist$TaxableIncome > PAlimit] - PAlimit))
      TIDist$PAincome <- pmin(TIDist$TaxableIncome, TIDist$PA)
      
      TIDist$SRincome <- NA
      TIDist$SRincome[TIDist$PAincome < TIDist$PA] <- 0 
      TIDist$SRincome[TIDist$PAincome >= TIDist$PA] <- pmin(TIDist$TaxableIncome[TIDist$PAincome >= TIDist$PA] - TIDist$PA[TIDist$PAincome >= TIDist$PA], SRthreshold)
      
      TIDist$BRincome <- NA
      TIDist$BRincome[TIDist$SRincome < SRthreshold] <- 0
      TIDist$BRincome[TIDist$SRincome >= SRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$SRincome >= SRthreshold] - TIDist$PA[TIDist$SRincome >= SRthreshold] - SRthreshold, BRthreshold)
      
      TIDist$IRincome <- NA
      TIDist$IRincome[TIDist$BRincome < BRthreshold] <- 0
      TIDist$IRincome[TIDist$BRincome >= BRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$BRincome >= BRthreshold] - TIDist$PA[TIDist$BRincome >= BRthreshold] - SRthreshold - BRthreshold, IRthreshold)
      
      TIDist$HRincome <- NA
      TIDist$HRincome[TIDist$IRincome < IRthreshold] <- 0
      TIDist$HRincome[TIDist$IRincome >= IRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$IRincome >= IRthreshold] - TIDist$PA[TIDist$IRincome >= IRthreshold] - SRthreshold - BRthreshold - IRthreshold, HRthreshold)
      
      TIDist$ARincome <- NA
      TIDist$ARincome[TIDist$HRincome < HRthreshold] <- 0
      TIDist$ARincome[TIDist$HRincome >= HRthreshold] <- TIDist$TaxableIncome[TIDist$HRincome >= HRthreshold] - TIDist$PA[TIDist$HRincome >= HRthreshold] - SRthreshold - BRthreshold - IRthreshold - HRthreshold
      
      # Calculate tax payable by tax bracket
      TIDist$SRtax <- SR * TIDist$SRincome
      TIDist$BRtax <- BR * TIDist$BRincome
      TIDist$IRtax <- IR * TIDist$IRincome
      TIDist$HRtax <- HR * TIDist$HRincome
      TIDist$ARtax <- AR * TIDist$ARincome
      
      
      
      # Calculate total income tax payable
  
      TIDist$TotalTax <- (TIDist$SRtax + TIDist$BRtax + TIDist$IRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
      total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
      paste("Total tax return from income tax = ", total_tax_sum)
    } else if (input$tax_choice == "current"){
      
      TIDist$PA <- NA
      TIDist$PA[TIDist$TaxableIncome <= PAlimit] <- PA
      TIDist$PA[TIDist$TaxableIncome > PAlimit] <- pmax(0, PA - 0.5 * (TIDist$TaxableIncome[TIDist$TaxableIncome > PAlimit] - PAlimit))
      TIDist$PAincome <- pmin(TIDist$TaxableIncome, TIDist$PA)
      
      TIDist$SRincome <- NA
                
      TIDist$BRincome <- NA
      TIDist$BRincome[TIDist$PAincome < TIDist$PA] <- 0 
      TIDist$BRincome[TIDist$PAincome >= TIDist$PA] <- pmin(TIDist$TaxableIncome[TIDist$PAincome >= TIDist$PA] - TIDist$PA[TIDist$PAincome >= TIDist$PA], BRthreshold)

      #TIDist$IRincome <- NA
           
      TIDist$HRincome <- NA
      TIDist$HRincome[TIDist$BRincome < BRthreshold] <- 0
      TIDist$HRincome[TIDist$BRincome >= BRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$BRincome >= BRthreshold] - TIDist$PA[TIDist$BRincome >= BRthreshold] - BRthreshold, HRthreshold - TIDist$PA[TIDist$BRincome >= BRthreshold])
      
      TIDist$ARincome <- NA
      TIDist$ARincome[TIDist$HRincome < HRthreshold] <- 0
      TIDist$ARincome[TIDist$HRincome >= HRthreshold] <- TIDist$TaxableIncome[TIDist$HRincome >= HRthreshold] - TIDist$PA[TIDist$HRincome >= HRthreshold] - BRthreshold - HRthreshold
      
      # Calculate tax payable by tax bracket
      TIDist$BRtax <- BR * TIDist$BRincome
      TIDist$HRtax <- HR * TIDist$HRincome
      TIDist$ARtax <- AR * TIDist$ARincome
      
      
      
      # Calculate total income tax payable
      
      TIDist$TotalTax <- ( TIDist$BRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
      total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
      paste("Total tax return from income tax = ", total_tax_sum)
      
      
    }
  })
}

shinyApp(ui = ui, server = server)
