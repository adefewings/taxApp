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
        .grey-out-council {
          pointer-events: none;
          background-color: # !important; /* Grey background color */
          color: #999999; /* Grey text color */
        }
        .grey-out-council .irs {
          pointer-events: none !important;
          opacity: 0.8;
        }
        .grey-out-council .irs-line {
          background: #999999 !important;
        }
        .grey-out-council .irs-bar {
          background: #999999 !important;
        }
        .grey-out-council .irs-handle {
          background: #999999 !important;
        }

        .input-group {
          display: flex;
          align-items: center;
          margin-bottom: 2px; /* Space between input groups */
        }
        .input-group-council {
        display: flex;
        align-items: center;
         margin-bottom: 2px;
        }
        .input-group-council label {
        margin-right: 5px; 
        }
        .input-group-council input {
        margin-left: 10px; 
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
          margin-right: 120px;
          }
          .calculate-button-container{
          background-color: #f0f0f0;
          padding: 10px;
          border-radius: 3px;
          margin-top: 10px;
          margin-left: 0px;
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
    tabPanel("Income Tax", value = "incomeTab", h4("Income tax is paid by employers to the gov via employee salary"),
             div(style = "height: 20px;", p("")), # Empty placeholder
             selectInput("tax_choice", "Select Income Tax System:",
                         choices = c("current", "scottish", "fully devolved"),
                         selected = "current"),
             div(style = "height: 30px;", p("")), # Empty placeholder
             fluidRow(
               column(6,
                      tabPanel("incomeTax",
                               div(class = "input-group",
                                   tags$label("Personal Allowance:", `for` = "PA"),
                                   numericInput("PA", NULL, 12500)),
                               
                               
                               #div(class = "input-group-council",
                              #   tags$label(id = "bandA-label","Band A limit", `for` = "bandA"),
                              #   numericInput("bandA", NULL, 0)),
                               
                               
                               
                               div(class = "input-group",
                                   tags$label("Personal Allowance Limit:", `for` = "PAlimit"),
                                   sliderInput("PAlimit", NULL, min = 0, max = 200000, value = 100000)),
                               div(class = "input-group",
                                   tags$label("Starter Rate Threshold:", `for` = "SRthreshold"),
                                   numericInput("SRthreshold", NULL, 2000)),
                               div(class = "input-group",
                                   tags$label(id="BRThreshold","Basic Rate Threshold:", `for` = "BRthreshold"),
                                   numericInput("BRthreshold", NULL, 0)),
                               div(class = "input-group",
                                   tags$label("Intermediate Rate Threshold:", `for` = "IRthreshold"),
                                   numericInput("IRthreshold", NULL, 8000)),
                               div(class = "input-group",
                                   tags$label(id="HRThreshold","Higher Rate Threshold:", `for` = "HRthreshold"),
                                   numericInput("HRthreshold", NULL, 0)),
                               div(class = "input-group",
                                   tags$label("Starter Rate:", `for` = "SR"),
                                   sliderInput("SR", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               div(class = "input-group",
                                   tags$label(id="BR","Basic Rate:", `for` = "BR"),
                                   sliderInput("BR", NULL, min = 0, max = 1, value = 0.2, step = 0.01)),
                               div(class = "input-group",
                                   tags$label("Intermediate Rate:", `for` = "IR"),
                                   sliderInput("IR", NULL, min = 0, max = 1, value = 0.21, step = 0.01)),
                               div(class = "input-group",
                                   tags$label(id="HR","Higher Rate:", `for` = "HR"),
                                   sliderInput("HR", NULL, min = 0, max = 1, value = 0.41, step = 0.01)),
                               div(class = "input-group",
                                   tags$label(id="AR","Additional Rate:", `for` = "AR"),
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
                     
                      div(class = "output-container",
                          verbatimTextOutput("totalTaxOutput")
                      ),
                      
                      
                      div(class = "output-container",
                          verbatimTextOutput("newTotalTaxOutput")
                      ),
                      div(class = "calculate-button-container",
                          actionButton("calculate", label = "update with new filters")
                      )
               )
             )        
    
             
    ),
    #Council Tax Tab:
    tabPanel("Council tax", value = "councilTax", h4("this is a tax levied on residential domestic property"),
             #empty gap first:
             div(style = "height: 20px;", p("")),
             #Need to have choices for Scotland, England and Wales:
             selectInput("councilTaxCountry", "Select Country for Council tax calculations",
                         choices = c("Wales", "Scotland", "England"),
                         selected = "Wales"),
             #Now need to include the different bands (A-H) with I only included for Wales:
             div(style = "height: 30px;", p("")), # Empty placeholder
             fluidRow(
               column(4,
                      tabPanel("councilTax",
                               div(class = "input-group-council",
                                   tags$label(id = "bandA-label","Band A limit", `for` = "bandA"),
                                   numericInput("bandA", NULL, 0)),
                               div(class = "input-group-council",
                                   tags$label(id="bandB-label","Band B limit", `for` = "bandB"),
                                   numericInput("bandB", NULL, 0)),
                               div(class = "input-group-council",
                                   tags$label(id="bandC-label","Band C limit", `for` = "bandC"),
                                   numericInput("bandC", NULL, 0)),
                               div(class = "input-group-council",
                                   tags$label(id="bandD-label","Band D limit", `for` = "bandD"),
                                   numericInput("bandD", NULL, 0)),
                               div(class = "input-group-council",
                                   tags$label(id = "bandE-label", "Band E limit",`for` = "bandE"),
                                   numericInput("bandE", NULL, 0)),
                               div(class = "input-group-council",
                                   tags$label(id="bandF-label","Band F limit", `for` = "bandF"),
                                   numericInput("bandF", NULL, 223000)),
                               div(class = "input-group-council",
                                   tags$label(id="bandG-label","Band G limit", `for` = "bandH"),
                                   numericInput("bandG", NULL, 324000)),
                               div(id = "bandH-container",
                                   div(class = "input-group-council",
                                   tags$label("Band H limit", `for` = "bandH"),
                                   numericInput("bandH", NULL, 424000))),
                      )
               ),
               column(5,
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      div(style = "height: 60px;", p("")), # Empty placeholder
                      
                      div(class = "output-container",
                          verbatimTextOutput("councilTaxOutput")
                      ),
                      
                      
                      #div(class = "output-container",
                      #    verbatimTextOutput("newTotalTaxOutput")
                      #),
                      #div(class = "calculate-button-container",
                       #   actionButton("calculate", label = "update with new filters")
                      #)
               )
             )  

             ),
    
    tabPanel("Non Domestic Rates", value = "more info", h4("This is a tax levied on non-domestic property"),
              div(style = "height: 20px;", p("")), # Empty placeholder
              selectInput("tax_choice", "Select Income Tax System:",
              choices = c("current", "scottish", "fully devolved"),
                selected = "current"),
             )
  )
)


server <- function(input, output, session) {
  #obersvations for Income tax:
  observe({
    enabled_ids <- character(0)
    disabled_ids <- character(0)
    
    if (input$tax_choice == "current") {
      enabled_ids <- c("BR", "HR", "AR")
      disabled_ids <- c("SR", "IR", "SRthreshold", "IRthreshold")
      updateNumericInput(session, "BRthreshold", value = 37500)
      updateNumericInput(session, "HRthreshold", value = 112500)
      updateSliderInput(session, "HR", min = 0, max = 1, value = 0.40,step = 0.01)
      updateSliderInput(session, "AR", min = 0, max = 1, value = 0.45,step = 0.01)
      #sliderInput("AR", NULL, min = 0, max = 1, value = 0.46, step = 0.01))
      
    } else if (input$tax_choice == "scottish" || input$tax_choice == "fully devolved") {
      enabled_ids <- c("SR", "BR", "IR", "HR", "AR", "SRthreshold", "IRthreshold")
      disabled_ids <- character(0)
      updateNumericInput(session, "BRthreshold", value = 11000)
      updateNumericInput(session, "HRthreshold", value = 107000)
      updateSliderInput(session, "HR", min = 0, max = 1, value = 0.41,step = 0.01)
      updateSliderInput(session, "AR", min = 0, max = 1, value = 0.46,step = 0.01)
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
  
  #obersvations for council tax (only activate band I for Wales):
  #observe({
   # enabled_ids_council <- character(0)
    #disabled_ids_council <- character(0)
    #if (input$councilTaxCountry == "Wales"){
     # enabled_ids_council <- c("bandI")
    #  disabled_ids_council <- character(0)
    #}else{
    #  enabled_ids_council <- character(0)
    #  disabled_ids_council <- c("bandI")
    #}
    #lapply(enabled_ids_council, function(id) {
    #  runjs(sprintf("$('#%s').prop('disabled', false).parent().removeClass('grey-out-council');", id))
    #})
    
    # Disable sliders
    #lapply(disabled_ids_council, function(id) {
    #  runjs(sprintf("$('#%s').prop('disabled', true).parent().addClass('grey-out-council');", id))
    #})
    
  #})
  
  observe({
    if (input$councilTaxCountry == "Wales") {
      show("bandH-container")
    } else {
      hide("bandH-container")
    }
  })
  
  
  #obervation to change the income tax band widths based on Scotland/Wales.
  observe({
    
    if (input$tax_choice == "current"){
      
    }else if (input$tax_choice == "scottish" || input$tax_choice == "fully devolved"){
      
    }
  })
  #tax_choice
  
  #obersvations to change the council tax bands in repsonse to what country choice:
  observe({
    if (input$councilTaxCountry == "Wales"){
      updateNumericInput(session, "bandA", value = 44000)
      updateNumericInput(session, "bandB", value = 65000)
      updateNumericInput(session, "bandC", value = 91000)
      updateNumericInput(session, "bandD", value = 123000)
      updateNumericInput(session, "bandE", value = 162000)
      updateNumericInput(session, "bandF", value = 223000)
      updateNumericInput(session, "bandG", value = 324000)
      #updateNumericInput(session, "bandH", value = 424000)
    }else if (input$councilTaxCountry == "Scotland"){
      updateNumericInput(session, "bandA", value = 27000)
      updateNumericInput(session, "bandB", value = 35000)
      updateNumericInput(session, "bandC", value = 45000)
      updateNumericInput(session, "bandD", value = 58000)
      updateNumericInput(session, "bandE", value = 80000)
      updateNumericInput(session, "bandF", value = 106000)
      updateNumericInput(session, "bandG", value = 212000)
      #updateNumericInput(session, "bandH", value = 10)
    }else{
      updateNumericInput(session, "bandA", value = 40000)
      updateNumericInput(session, "bandB", value = 52000)
      updateNumericInput(session, "bandC", value = 68000)
      updateNumericInput(session, "bandD", value = 88000)
      updateNumericInput(session, "bandE", value = 120000)
      updateNumericInput(session, "bandF", value = 160000)
      updateNumericInput(session, "bandG", value = 320000)
    }
  })
  
latest_value <- reactiveVal(0)
saved_value <- reactiveVal(0)
  
  calculateTax <- function(){
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
      #latest_value(total_tax_sum)
      #paste("Total tax return from income tax = ", total_tax_sum)
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
      #latest_value(total_tax_sum)
      #paste("tax return from income tax with current filters= ", total_tax_sum)
      
      
    }
    total_tax_sum
  }
  
  
  output$totalTaxOutput <- renderText({
    total_tax_sum <- calculateTax()
    latest_value(total_tax_sum)
    paste("Total income tax return = ", total_tax_sum, "(", round(total_tax_sum/1000000000, digits = 2), " billion)")
  })
  
  output$newTotalTaxOutput <- renderText({
    newAmount <- saved_value()
    paste("Previous income tax return = ", newAmount, "(", round(newAmount/1000000000, digits = 2), " billion)")
  })
  
  output$councilTaxOutput <- renderText({
    paste("Council Tax return = ", "need info Data")
  })
  
  observeEvent(input$calculate, {
    # Save the current latest value when the button is clicked
    saved_value(latest_value())
  })
  
}

shinyApp(ui = ui, server = server)
