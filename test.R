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
        .input-group-ltt {
          display: flex;
          align-items: center;
          margin-bottom: 1px;
        }
        .input-group-ltt label{
          width: 250px;
          margin-right: 5px;
        }
        .input-group-ltt input {
          width: 100px; 
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
        .input-group-ndr{
        display: flex;
        align-items: center;
        margin-bottom: 2px;
        }
        .input-group-ndr label {
        width: 250px;
        margin-right: 5px;
        }
        .input-group-ndr input {
          width: 100px;
        }
        .input-group-landfill {
          display: flex;
          align-items: center;
          margin-bottom: 10px;
        }
        .input-group-landfill label{
          width: 100px;
        margin-right: 5px;
        }
       .input-group-landfill input {
          width: 100px;
        }
        .ndr-selection {
        display: flex;
        align-items: center;
        margin-right: 5px;
        }
        .ndr-slection label{
        width: 150px;
        }
        .ndr-selection input{
        width: 50px;
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
        .radio-inline {
            font-size: 20px;
        }
        label {
          font-size: 20px;
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
               column(5,
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
             div(style = "ndr-selection",
                 selectInput("ndrCountry", "Select Income Tax System:",
                             choices = c("Wales", "Scotland", "England"),
                             selected = "Wales")),
             div(style = "height: 20px;", p("")), # Empty placeholder
             fluidRow(
               column(6,
                      tabPanel("NDR",
                               
                               div(class = "input-group-ndr",
                                   tags$label(id = "rateableValue-label","Proposed change for rateable values:", `for` = "rateableValueChange"),
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
             
    ),
    
    tabPanel("Land Transactional Tax", value = "more info", h4("This is a tax on property transactions in Wales. Replaced Stamp Duty which is still in place in England"),
             div(style = "height: 20px;", p("")), # Empty placeholder
             
             selectInput("LTTchoice", "Select Income Tax System:",
                         choices = c("Wales", "Scotland", "England"),
                         selected = "Wales"),
             div(style = "height: 20px;", p("")), # Empty placeholder
             
             fluidRow(
               column(3,
                      tabPanel("LTT",
                               tags$p("Residential Property Tax", style = "font-size: 21px;font-weight: bold;margin-left: 70px;"),
                               tags$p("Band 1:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band1Limit","Band 1 Limit:", `for` = "band1LimitRes"),
                                   numericInput("band1LimitRes", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate1Res"),
                                   sliderInput("LTTrate1Res", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 2:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band2Limit","Band 2 Limit:", `for` = "band2LimitRes"),
                                   numericInput("band2LimitRes", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrateRes"),
                                   sliderInput("LTTrate2Res", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 3:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band3Limit","Band 3 Limit:", `for` = "band3LimitRes"),
                                   numericInput("band3LimitRes", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate3Res"),
                                   sliderInput("LTTrate3Res", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 4:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band4Limit","Band 4 Limit:", `for` = "band4LimitRes"),
                                   numericInput("band4LimitRes", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate4Res"),
                                   sliderInput("LTTrate4Res", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 5:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band5Limit","Band 5 Limit:", `for` = "band5LimitRes"),
                                   numericInput("band5LimitRes", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate5Res"),
                                   sliderInput("LTTrate5Res", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 6:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate6Res"),
                                   sliderInput("LTTrate6Res", NULL, min = 0, max = 1, value = 0.19, step = 0.01))
                               
                      )
               ),
               column(3,
                      tabPanel("LTT",
                               tags$p("Non-Residential Property Tax (Purchase)", style = "font-size: 21px;font-weight: bold;margin-left: 10px;"),
                               tags$p("Band 1:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band1Limit","Band 1 Limit:", `for` = "band1LimitPurchase"),
                                   numericInput("band1LimitPurchase", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate1Purchase"),
                                   sliderInput("LTTrate1Purchase", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 2:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band2Limit","Band 2 Limit:", `for` = "band2LimitPurchase"),
                                   numericInput("band2LimitPurchase", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTratePurchase"),
                                   sliderInput("LTTrate2Purchase", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 3:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band3Limit","Band 3 Limit:", `for` = "band3LimitPurchase"),
                                   numericInput("band3LimitPurchase", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate3Purchase"),
                                   sliderInput("LTTrate3Purchase", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 4:", style = "font-size: 18px;font-weight: bold;"),
                               
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate4Purchase"),
                                   sliderInput("LTTrate4Purchase", NULL, min = 0, max = 1, value = 0.19, step = 0.01))
                               
                               
                      )
               ),
                column(3,
                      tabPanel("LTT",
                               tags$p("Non-Residential Property Tax (Lease)", style = "font-size: 21px;font-weight: bold;margin-left: 10px;"),
                               tags$p("Band 1:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band1Limit","Band 1 Limit:", `for` = "band1LimitLease"),
                                   numericInput("band1LimitLease", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate1Lease"),
                                   sliderInput("LTTrate1Lease", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 2:", style = "font-size: 18px;font-weight: bold;"),
                               div(class = "input-group-ltt",
                                   tags$label(id="band2Limit","Band 2 Limit:", `for` = "band2LimitLease"),
                                   numericInput("band2LimitLease", NULL, 0)),
                               div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrateLease"),
                                   sliderInput("LTTrate2Lease", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               tags$p("Band 3:", style = "font-size: 18px;font-weight: bold;"),
                                  div(class = "input-group-ltt",
                                   tags$label("LTT rate", `for` = "LTTrate3Lease"),
                                   sliderInput("LTTrate3Lease", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                               
                               
                               
                               
                      )
             )
             
    )
  ),
  tabPanel("Landfill Disposal tax", value = "more info", h4("Levy paid by Landfill owners, usually pased on to those paying to use the landfill"),
           div(style = "height: 20px;", p("")), # Empty placeholder
           
           selectInput("landfillChoice", "Select Income Tax System:",
                       choices = c("Wales", "Scotland", "England"),
                       selected = "Wales"),
           radioButtons("landfillButtonChoice", "Please select a Country", choices = c("Wales", "Scotland", "England"), selected = "Wales", inline = FALSE,
                        width = NULL),
           div(style = "height: 20px;", p("")), # Empty placeholder
           
           fluidRow(
             column(6,
                    tabPanel("landfills",
                             
                             #tags$p("Band 1:", style = "font-size: 18px;font-weight: bold;"),
                             
                             div(class = "input-group-landfill",
                                 tags$label("Lower Rate:", `for` = "lowRateLandfill"),
                                 sliderInput("lowRateLandfill", NULL, min = 0, max = 200, value = 3.1 , step = 0.1)),
                             
                             
                             div(class = "input-group-landfill",
                                 tags$label("Standard rate", `for` = "stdRateLandfill"),
                                 sliderInput("stdRateLandfill", NULL, min = 0, max = 200, value = 98.6, step = 0.1)),
                             
                            
                             
                             
                             
                    )
             ),
             
             
           )
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
  
  
  observe({
    if (input$LTTchoice == "Wales"){
      updateNumericInput(session, "band1LimitRes", value = 180000)
      updateNumericInput(session, "band2LimitRes", value = 250000)
      updateNumericInput(session, "band3LimitRes", value = 400000)
      updateNumericInput(session, "band4LimitRes", value = 750000)
      updateNumericInput(session, "band5LimitRes", value = 1500000)
      updateSliderInput(session, "LTTrate1Res", min = 0, max = 1, value = 0,step = 0.01)
      updateSliderInput(session, "LTTrate2Res", min = 0, max = 1, value = 0.035,step = 0.01)
      updateSliderInput(session, "LTTrate3Res", min = 0, max = 1, value = 0.05,step = 0.01)
      updateSliderInput(session, "LTTrate4Res", min = 0, max = 1, value = 0.075,step = 0.01)
      updateSliderInput(session, "LTTrate5Res", min = 0, max = 1, value = 0.1,step = 0.01)
      updateSliderInput(session, "LTTrate6Res", min = 0, max = 1, value = 0.12,step = 0.01)
      updateNumericInput(session, "band1LimitPurchase", value = 225000)
      updateNumericInput(session, "band2LimitPurchase", value = 250000)
      updateNumericInput(session, "band3LimitPurchase", value = 1000000)
      updateSliderInput(session, "LTTrate1Purchase", min = 0, max = 1, value = 0,step = 0.01)
      updateSliderInput(session, "LTTrate2Purchase", min = 0, max = 1, value = 0.01,step = 0.01)
      updateSliderInput(session, "LTTrate3Purchase", min = 0, max = 1, value = 0.05,step = 0.01)
      updateSliderInput(session, "LTTrate4Purchase", min = 0, max = 1, value = 0.06,step = 0.01)
      updateNumericInput(session, "band1LimitLease", value = 225000)
      updateNumericInput(session, "band2LimitLease", value = 2000000)
      updateSliderInput(session, "LTTrate1Lease", min = 0, max = 1, value = 0,step = 0.01)
      updateSliderInput(session, "LTTrate2Lease", min = 0, max = 1, value = 0.01,step = 0.01)
      updateSliderInput(session, "LTTrate3Lease", min = 0, max = 1, value = 0.02,step = 0.01)
    }else if (input$LTTchoice == "England"){
      updateNumericInput(session, "band1LimitRes", value = 200000)
      updateNumericInput(session, "band2LimitRes", value = 260000)
      updateNumericInput(session, "band3LimitRes", value = 500000)
      updateNumericInput(session, "band4LimitRes", value = 900000)
      updateNumericInput(session, "band5LimitRes", value = 1800000)
      updateSliderInput(session, "LTTrate1Res", min = 0, max = 1, value = 0,step = 0.01)
      updateSliderInput(session, "LTTrate2Res", min = 0, max = 1, value = 0.03,step = 0.01)
      updateSliderInput(session, "LTTrate3Res", min = 0, max = 1, value = 0.05,step = 0.01)
      updateSliderInput(session, "LTTrate4Res", min = 0, max = 1, value = 0.08,step = 0.01)
      updateSliderInput(session, "LTTrate5Res", min = 0, max = 1, value = 0.11,step = 0.01)
      updateSliderInput(session, "LTTrate6Res", min = 0, max = 1, value = 0.14,step = 0.01)
      updateNumericInput(session, "band1LimitPurchase", value = 240000)
      updateNumericInput(session, "band2LimitPurchase", value = 280000)
      updateNumericInput(session, "band3LimitPurchase", value = 1500000)
      updateSliderInput(session, "LTTrate1Purchase", min = 0, max = 1, value = 0,step = 0.01)
      updateSliderInput(session, "LTTrate2Purchase", min = 0, max = 1, value = 0.02,step = 0.01)
      updateSliderInput(session, "LTTrate3Purchase", min = 0, max = 1, value = 0.04,step = 0.01)
      updateSliderInput(session, "LTTrate4Purchase", min = 0, max = 1, value = 0.05,step = 0.01)
      updateNumericInput(session, "band1LimitLease", value = 200000)
      updateNumericInput(session, "band2LimitLease", value = 2500000)
      updateSliderInput(session, "LTTrate1Lease", min = 0, max = 1, value = 0,step = 0.01)
      updateSliderInput(session, "LTTrate2Lease", min = 0, max = 1, value = 0.01,step = 0.01)
      updateSliderInput(session, "LTTrate3Lease", min = 0, max = 1, value = 0.02,step = 0.01)
      }
  })
  
  
  #lowRateLandfill/std
  
  
  observe({
    if (input$landfillButtonChoice == "Wales"){
      updateSliderInput(session, "lowRateLandfill",  NULL, min = 0, max = 200, value = 3.1 , step = 0.1)
      updateSliderInput(session, "stdRateLandfill",  NULL, min = 0, max = 200, value = 98.6 , step = 0.1)
    } else if (input$landfillButtonChoice == "England"){
      updateSliderInput(session, "lowRateLandfill",  NULL, min = 0, max = 200, value = 6.1 , step = 0.1)
      updateSliderInput(session, "stdRateLandfill",  NULL, min = 0, max = 200, value = 80 , step = 0.1)
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