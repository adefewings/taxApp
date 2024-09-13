#################################
#Bangor Business school tax app #
#################################
#Last edited: 27/08/24          #
#Edited by: Ollie Barbaresi     #
#eResearch                      #
#################################


#import libraries:
library(shiny)
library(shinyjs)
library(plotly) 
library(ggplot2)

#############################
#############################
#############################
#Code to allow translation: #
#############################
#############################
#############################
#input csv file which contains the dictionary of words for translations:
text_data <- read.csv("translations.csv", stringsAsFactors = FALSE)

#  convert csv file date  dataframe to the nested list
text_resources_func <- function(df) {
  # Ensure 'key', 'english', and 'welsh' columns exist
  if (!all(c("key", "english", "welsh") %in% names(df))) {
    stop("CSV file must contain 'key', 'english', and 'welsh' columns.")
  }
  
  # Extract unique keys
  keys <- df$key
  
  # Create lists for each language
  english_list <- setNames(as.list(df$english), keys)
  welsh_list <- setNames(as.list(df$welsh), keys)
  
  # Combine into a named list
  list(
    English = english_list,
    Welsh = welsh_list
  )
}

#Function call to enable translations
text_resources <- text_resources_func(text_data)

########################
########################
########################
#UI Function:          #
########################
########################
########################
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  #Custom css for formatting app items:
  #These include the inputs, backgrounds and some formatting
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #FFFFFF;
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
         margin-top:10px;
          margin-bottom: 30px;
        }
        .input-group-council label {
        width: 200px;
        margin-right: 5px; 
      
        }
        .input-group-council input {
        width: 100px; 
        }
        
        
        .council-slider {
          display: flex;
          align-items: center;
          margin-bottom: 5px; /* Space between input groups */
        }
        .council-slider label {
          flex: 0 0 100px; /* Fixed width for the label (adjust as needed) */
          margin-right: 5px; /* Space between label and slider */
          font-size: 14px; /* Font size for the label */
        }
        .council-slider slider {
              flex: 1; /* Takes up the remaining space */
    /* Optional: Styling the slider */
    -webkit-appearance: none; /* Remove default styling */
    height: 8px; /* Height of the slider track */
    background: #ddd; /* Background color of the track */
    border-radius: 4px; /* Rounded corners */
    outline: none; /* Remove outline on focus */
        }
        .council-slider .shiny-input-container {
          flex-grow: 0.2; /* Let inputs take the remaining space */
          font-size: 14px;
        }
        .council-slider.custom-numeric-input .shiny-input-container input {
          font-size: 14px; /* Adjust font size for the numeric input field */
        }
        
        
        
        
        
        .pa_input_left {
          display: flex;
          align-items: center;
          margin-top:10px;
          margin-bottom: 30px; /* Space between input groups */
         
        }
        .pa_input_left label {
          flex: 0 0 130px; /* Fixed width for labels */
          margin-right: 5px; /* Space between label and input */
          font-size: 14px;
          font-weight: normal;
        }
        .pa_input_left input {
          width: 100%; /* Ensures the input is as wide as its container */
          max-width: 65px; /* Optional: Set a maximum width for the input */
          padding: 2px; /* Padding inside the input box */
          font-size: 14px; /* Font size for the input text */
          box-sizing: border-box; /* Ensures padding is included in the total width */
        }
        
       .pa_input_right {
          display: flex;
          align-items: center;
          margin-top:10px;
          margin-bottom: 30px; /* Space between input groups */
         
        }
        .pa_input_right label {
          flex: 0 0 75px; /* Fixed width for labels */
          margin-right: 5px; /* Space between label and input */
          font-size: 14px;
          font-weight: normal;
        }
        .pa_input_right input {
          width: 100%; /* Ensures the input is as wide as its container */
          max-width: 80px; /* Optional: Set a maximum width for the input */
          padding: 2px; /* Padding inside the input box */
          font-size: 14px; /* Font size for the input text */
          box-sizing: border-box; /* Ensures padding is included in the total width */
        }
        
        .rates_input_left {
          display: flex;
          align-items: center;
          margin-top:13px;
          margin-bottom: 30px; /* Space between input groups */
          margin-right:0px;
         
        }
        .rates_input_left label {
          flex: 0 0 115px; /* Fixed width for labels */
          margin-right: 3px; /* Space between label and input */
          font-size: 14px;
          font-weight: normal;
          
        }
        .rates_input_left input {
          width: 100%; /* Ensures the input is as wide as its container */
          max-width: 65px; /* Optional: Set a maximum width for the input */
          padding: 0px; /* Padding inside the input box */
          margin-top:8px;
          line-height: 14px;
          font-size: 14px; /* Font size for the input text */
          box-sizing: border-box; /* Ensures padding is included in the total width */
          margin-right: 0px;
          height: 25px;
        }
       
        .rates_input_middle {
          display: flex;
          align-items: center;
          margin-top:10px;
          margin-bottom: 30px; /* Space between input groups */
          margin-left: 0px;
         
        }
        .rates_input_middle label {
          flex: 0 0 25px; /* Fixed width for labels */
          margin-right: 5px; /* Space between label and input */
          font-size: 14px;
          font-weight: normal;
          margin-left: 0px;
        }
        .rates_input_middle input {
          width: 100%; /* Ensures the input is as wide as its container */
          max-width: 65px; /* Optional: Set a maximum width for the input */
          padding: 2px; /* Padding inside the input box */
          font-size: 14px; /* Font size for the input text */
          box-sizing: border-box; /* Ensures padding is included in the total width */
        }
       .rates_input_right_slider {
          display: flex;
          align-items: center;
          margin-bottom: 5px; /* Space between input groups */
        }
        .rates_input_right_slider label {
          flex: 0 0 90px; /* Fixed width for the label (adjust as needed) */
          margin-right: 5px; /* Space between label and slider */
          font-size: 14px; /* Font size for the label */
          font-weight: normal;
        }
        .rates_input_right_slider slider {
              flex: 1; /* Takes up the remaining space */
    /* Optional: Styling the slider */
    -webkit-appearance: none; /* Remove default styling */
    height: 8px; /* Height of the slider track */
    background: #ddd; /* Background color of the track */
    border-radius: 4px; /* Rounded corners */
    outline: none; /* Remove outline on focus */
        }
        .rates_input_right_slider .shiny-input-container {
          flex-grow: 0.2; /* Let inputs take the remaining space */
          font-size: 14px;
        }
        .rates_input_right_slider.custom-numeric-input .shiny-input-container input {
          font-size: 14px; /* Adjust font size for the numeric input field */
        }
        
        .grey-text-box {
        background-color: #f0f0f0; /* Light grey background */
        padding: 2px; /* Add some padding */
        border: 1px solid #d3d3d3; /* Optional: Light grey border */
        border-radius: 5px; /* Optional: Rounded corners */
        color: #333; /* Optional: Text color */
        font-size: 13px;
        width: 90px;
      }
       
        
      
        
        
        
        
        
        
       .input-group-thresholds {
          display: flex;
          align-items: center;
          margin-top:10px;
          margin-bottom: 30px; /* Space between input groups */
         
        }
        .input-group-thresholds label {
          flex: 0 0 200px; /* Fixed width for labels */
          margin-right: 5px; /* Space between label and input */
          font-size: 14px;
        }
        .input-group-thresholds input {
          width: 100%; /* Ensures the input is as wide as its container */
    max-width: 100px; /* Optional: Set a maximum width for the input */
    padding: 5px; /* Padding inside the input box */
    font-size: 14px; /* Font size for the input text */
    box-sizing: border-box; /* Ensures padding is included in the total width */
        }
        
        .input-group-thresholds.custom-numeric-input .shiny-input-container input {
          font-size: 14px; /* Adjust font size for the numeric input field */
        }
        .input-group-block {
          display: flex;
          align-items: center;
          margin-top:10px;
          margin-bottom: 5px; /* Space between input groups */
         
        }
        .input-group-block label {
          flex: 0 0 200px; /* Fixed width for labels */
          margin-right: 5px; /* Space between label and input */
          font-size: 15px;
        }
        .input-group-block input {
          width: 100%; /* Ensures the input is as wide as its container */
    max-width: 100px; /* Optional: Set a maximum width for the input */
    padding: 5px; /* Padding inside the input box */
    font-size: 15px; /* Font size for the input text */
    box-sizing: border-box; /* Ensures padding is included in the total width */
        }
        
        .input-group-block-numeric-input .shiny-input-container input {
          font-size: 16px; /* Adjust font size for the numeric input field */
        }
        
        .input-group-slider {
          display: flex;
          align-items: center;
          margin-bottom: 5px; /* Space between input groups */
        }
        .input-group-slider label {
          flex: 0 0 70px; /* Fixed width for the label (adjust as needed) */
          margin-right: 5px; /* Space between label and slider */
          font-size: 14px; /* Font size for the label */
        }
        .input-group-slider slider {
              flex: 1; /* Takes up the remaining space */
    /* Optional: Styling the slider */
    -webkit-appearance: none; /* Remove default styling */
    height: 8px; /* Height of the slider track */
    background: #ddd; /* Background color of the track */
    border-radius: 4px; /* Rounded corners */
    outline: none; /* Remove outline on focus */
        }
        .input-group-slider .shiny-input-container {
          flex-grow: 0.2; /* Let inputs take the remaining space */
          font-size: 14px;
        }
        .input-group-slider.custom-numeric-input .shiny-input-container input {
          font-size: 14px; /* Adjust font size for the numeric input field */
        }
       
        .input-group-ndr {
            display: flex;
            align-items: center;
            margin-bottom: 15px; /* Increased spacing between input groups */
            padding: 5px; /* Added padding for better spacing */
            border: 1px solid #ddd; /* Light border to define input group */
            border-radius: 5px; /* Rounded corners for a softer look */
            background-color: #f9f9f9; /* Light background for contrast */
        }


        .input-group-ndr label {
            width: 1000px; /* Adjusted to a more standard width */
            margin-right: 15px; /* Increased margin for better separation */
            font-weight: bold; /* Make labels more prominent */
            text-align: left; /* Align labels to the right for consistency */
        }
        
        
        .input-group-ndr input {
            /*flex: 1;  Allow inputs to take up remaining space */
            margin-right: 0; /* Removed margin-right to use full width */
            padding: 8px; /* Added padding for better input readability */
            border: 1px solid #ccc; /* Light border for inputs */
            border-radius: 4px; /* Rounded corners for inputs */
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
          .output1-container {
       
        background-color: #ffffff; /* Light background color */
        width: 300px; /* Make sure it uses the full width available */
        box-sizing: border-box; /* Ensure padding and border are included in the total height */
        display: flex; /* Use Flexbox to manage layout */
          }
          .calculate-button-container{
          background-color: #FFFFFF;
          padding: 5px; 
          border-radius: 3px;
          margin-top: 10px;
          margin-bottom: 10px;
          margin-left: 0px;
          marin-right:100px;
          display: inline-block; 
          padding-left: 20px; 
          padding-right: 20px;
          }
        .radio-inline {
            font-size: 15px;
        }
        label {
          font-size: 15px;
        }
        #title {
      font-size: 35px;
      font-weight: bold;
        }
        #piechart_title {
        font-size: 25px;
        font-weight: bold;
        }
        #council_tax_title {
        font-size: 16px;
        font-weight: bold;
        }
      
      ")
    )
  ),
  #end of CSS
  
  
  
  #Start of the main page - default is the income tax page.
  #Title and translate button on top of screen
  fluidRow(
    column(4,
           align = "left",
           tags$img(src = "businessLogo.png", height = "100px", width = "auto"),
           
           ),
    column(8,
           align = "right",
           #The formatted rectangles for the top of the screen:
           tags$div(style = "background-color: #C50031; height: 20px; width: 100%; margin: 10px 0; padding: 0;"),
           tags$div(style = "background-color: #F6BC0A; height: 20px; width: 90%; margin: 10px 0; padding: 0;"),
           #tags$div(style = "background-color: #C50031; height: 20px; width: 80%; margin: 10px 0; padding: 0;"),
           actionButton("contact_us_button", label = textOutput("contact_us_button")),
           actionButton("translateButton", textOutput("translate_button"))

           ),
    
  ),
  
  fluidRow(
    column(7, 
           titlePanel(textOutput("title"))
    ),
    
    
  ),
  #br(),
 
  fluidRow(
    column(5,
           #Main app intro/instructions are going in this section
           align = "left",
           
          
          tags$div(style = "font-size: 16px;",textOutput("main_app_intro")),
          div(style = "height: 30px;", p("")),
          tabsetPanel(
            id = "tabs",
            ###################################
            #income taxes tab                 #  
            ###################################
            tabPanel(textOutput("income_tax"), value = "incomeTab"),
            tags$div(style = "font-size: 16px;",textOutput("income_tax_intro"),
                     div(style = "height: 20px;", p("")), # Empty placeholder
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     ##Start with NDR:
                     #div(class = "input-group-block custom-numeric-input",
                      #   tags$label(textOutput("block_grant_input"), `for` = "block_grant_total"),
                       #  numericInput("block_grant_total", NULL, 19000, step = 100)),
                     
                    # tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     
                     fluidRow(column(5,
                                     radioButtons("income_tax_system_choice", textOutput("select_income_system"), 
                                                  choices = c("Current Settlement", "Scottish Model", "Full Devolution"), 
                                                  selected = "Current Settlement", 
                                                  inline = FALSE,
                                                  width = NULL)
                     ),
                     column(6,
                            div(style = "height: 20px;", p("")),
                            div(class = "output1-container",
                                verbatimTextOutput("totalTaxOutput")
                            )
                     )
                     ),
                    #adding the new requested rows and columns for the inputs:
                    numericInput("num_rows", "Number of bands", value = 3, min = 3, max = 10),
                    fluidRow(
                      column(5,
                             div(class = "pa_input_left",
                                 tags$label("Personal Allowance:", `for` = "PAnew"),
                                 numericInput("PAnew", NULL, 12500, step = 100)),
                      ),
                      
                      column(3,
                             div(style = "height: 20px;", p(""))
                             ),
                      
                      column(4,
                             div(class = "pa_input_right",
                                 tags$label("PA Limit:", `for` = "pa_limit"),
                                 numericInput("pa_limit", NULL, 100000, step = 100)),
                             
                      ),
                    ),
                    
                    
                    fluidRow(
                      column(4,
                             div(class = "rates_input_left",
                                 tags$label("Rate 1 Threshold:", `for` = "rate_1_t"),
                                 numericInput("rate_1_t", NULL, 50270, step = 100)),

                      ),
                             
                      column(2,
                             div(style = "height: 12px;", p("")),
                             div(class = "grey-text-box", 
                                 textOutput("uk_rate_text_1")
                             )
                             ), 
                      
                      column(6,
                             div(class = "rates_input_right_slider custom-numeric-input",
                                 tags$label("Welsh rate:", `for` = "welsh_rate_1"),
                                 sliderInput("welsh_rate_1", NULL, min = 0, max = 100, value = 20, step = 1)),)
                             
                      ),
                    fluidRow(
                      column(4,
                             div(class = "rates_input_left",
                                 tags$label("Rate 2 Threshold:", `for` = "rate_2_t"),
                                 numericInput("rate_2_t", NULL, 100270, step = 100)),
                             
                      ),
                      
                      column(2,
                             div(style = "height: 12px;", p("")),
                             div(class = "grey-text-box", 
                                 textOutput("uk_rate_text_2")
                             )
                      ), 
                      
                      column(6,
                             div(class = "rates_input_right_slider custom-numeric-input",
                                 tags$label("Welsh rate:", `for` = "welsh_rate_2"),
                                 sliderInput("welsh_rate_2", NULL, min = 0, max = 100, value = 30, step = 1)),)
                      
                    ),
                    fluidRow(
                      column(4,
                             div(class = "rates_input_left",
                                 tags$label("Rate 3 Threshold:", `for` = "rate_3_t"),
                                 numericInput("rate_3_t", NULL, 100270, step = 100)),
                             
                      ),
                      
                      column(2,
                             div(style = "height: 12px;", p("")),
                             div(class = "grey-text-box", 
                                 textOutput("uk_rate_text_3")
                             )
                      ), 
                      
                      column(6,
                             div(class = "rates_input_right_slider custom-numeric-input",
                                 tags$label("Welsh rate:", `for` = "welsh_rate_3"),
                                 sliderInput("welsh_rate_3", NULL, min = 0, max = 100, value = 35, step = 1)),)
                      
                    ),
                    
                    uiOutput("dynamic_rows"),
                    
                    div(class = "output1-container",
                        verbatimTextOutput("new_income_tax")
                    ),
                     
                     div(style = "height: 30px;", p("")), # Empty placeholder
                    tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                    
                     fluidRow(#input-group-thresholds
                       column(6,
                              div(class = "input-group-thresholds custom-numeric-input",
                                  tags$label(textOutput("pa_box"), `for` = "PA"),
                                  numericInput("PA", NULL, 12570, step = 100)),
                              div(class = "input-group-thresholds custom-numeric-input",
                                  tags$label(textOutput("sr_threshold"), `for` = "SRthreshold"),
                                  numericInput("SRthreshold", NULL, 2000, step = 100)),
                              div(class = "input-group-thresholds custom-numeric-input",
                                  tags$label(id="BRThreshold",textOutput("br_threshold"), `for` = "BRthreshold"),
                                  numericInput("BRthreshold", NULL, 0, step = 100)),
                              div(class = "input-group-thresholds custom-numeric-input",
                                  tags$label(textOutput("ir_threshold"), `for` = "IRthreshold"),
                                  numericInput("IRthreshold", NULL, 18000, step = 100)),
                              div(class = "input-group-thresholds custom-numeric-input",
                                  tags$label(id="HRThreshold",textOutput("hr_threshold"), `for` = "HRthreshold"),
                                  numericInput("HRthreshold", NULL, 0, step = 100)),
                              
                       ),
                       column(6,
                              div(class = "input-group-slider custom-numeric-input",
                                  tags$label(textOutput("pa_limit"), `for` = "PAlimit"),
                                  sliderInput("PAlimit", NULL, 
                                              min = 0, 
                                              max = 200000, 
                                              value = 100000,
                                              #animate = TRUE,
                                              ticks = TRUE,
                                              step = 100
                                              #breaks = seq(0, 200000, by = 20000)
                                              #labels = scales::comma(seq(0, 200000, by = 20000))
                                  )),
                              div(class = "input-group-slider custom-numeric-input",
                                  tags$label(textOutput("sr"), `for` = "SR"),
                                  sliderInput("SR", NULL, min = 0, max = 1, value = 0.19, step = 0.01)),
                              div(class = "input-group-slider custom-numeric-input",
                                  tags$label(id="BR",textOutput("br"), `for` = "BR"),
                                  sliderInput("BR", NULL, min = 0, max = 1, value = 0.2, step = 0.01)),
                              div(class = "input-group-slider custom-numeric-input",
                                  tags$label(textOutput("ir"), `for` = "IR"),
                                  sliderInput("IR", NULL, min = 0, max = 1, value = 0.21, step = 0.01)),
                              div(class = "input-group-slider custom-numeric-input",
                                  tags$label(id="HR",textOutput("hr"), `for` = "HR"),
                                  sliderInput("HR", NULL, min = 0, max = 1, value = 0.41, step = 0.01)),
                              div(class = "input-group-slider custom-numeric-input",
                                  tags$label(id="AR",textOutput("ar"), `for` = "AR"),
                                  sliderInput("AR", NULL, min = 0, max = 1, value = 0.46, step = 0.01))
                       )
                       
                       
                     ),
                     fluidRow(
                       column(10,
                              
                              #style = "border: 2px solid #007bff; padding: 10px; margin: 5px; border-radius: 5px;",
                              div(style = "height: 60px;", p("")), # Empty placeholder
                              br(),
                              
                              actionButton("calculate", label = textOutput("update_with_new_filters"))
                       ),
                       plotlyOutput("pieChart"),
                       div(class = "calculate-button-container",
                           actionButton("toggleButton", label = textOutput("divide_by_people")),
                           actionButton("viewButton", label = textOutput("show_breakdown"))
                       ),
                       plotlyOutput("stackedPlot"),
                       #button and output to temp update the tax:
                       #div(class = "output-container",
                       #     verbatimTextOutput("newTotalTaxOutput")
                       # ),
                       #div(class = "calculate-button-container",
                       #)
                     )        
                     
            ),
            ###################################
            #Local taxes tab                  #  
            ###################################
            tabPanel(textOutput("local_taxes_tab_label"), value = "councilTax", h4(textOutput("local_taxes_tab_intro")),
                     #empty gap first:
                     div(style = "height: 20px;", p("")),
                     radioButtons("councilTaxCountry", textOutput("local_tax_system_selection"), 
                                  choices = c("current", "scottish", "fully devolved"), 
                                  selected = "current", 
                                  inline = FALSE,
                                  width = "400px"),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     
                     #Start with NDR:
                     div(class = "input-group-council",
                         tags$label(textOutput("ndr_input"), `for` = "ndrTotal"),
                         numericInput("ndrTotal", NULL, 1000)),
                     br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label(textOutput("tourism_input"), `for` = "tourism_levy_total"),
                         numericInput("tourism_levy_total", NULL, 0)),
                     br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     titlePanel(textOutput("council_tax_title")),
                     
                     fluidRow(
                       column(5,
                              div(class = "input-group-council",
                                  tags$label("Top Band = ", `for` = "topBandValue"),
                                  numericInput("topBandValue", NULL, 2000)),
                       ),
                       column(6,
                              div(class = "output-container",
                                  verbatimTextOutput("councilTaxOutput")
                              )
                       )
                     ),
                     
                     fluidRow(
                       column(5,
                              #border for the fluid row/column
                              #style = "border: 2px solid #007bff; padding: 10px; margin: 5px; border-radius: 5px;",
                              tabPanel("councilTax",
                                       #little extra gap for alignment
                                       div(style = "height: 2px;", p("")), 
                                       div(class = "input-group-council",
                                           tags$label(id = "bandA-label","Band A limit =", `for` = "bandA"),
                                           numericInput("bandA", NULL, 0)),
                                       
                                       div(class = "input-group-council",
                                           tags$label(id="bandB-label","Band B limit =", `for` = "bandB"),
                                           numericInput("bandB", NULL, 0)),
                                       
                                       div(class = "input-group-council",
                                           tags$label(id="bandC-council","Band C limit =", `for` = "bandC"),
                                           numericInput("bandC", NULL, 0)),
                                       
                                       div(class = "input-group-council",
                                           tags$label(id="bandD-label","Band D limit =", `for` = "bandD"),
                                           numericInput("bandD", NULL, 0)),
                                       
                                       div(class = "input-group-council",
                                           tags$label(id = "bandE-label", "Band E limit =",`for` = "bandE"),
                                           numericInput("bandE", NULL, 0)),
                                       
                                       div(class = "input-group-council",
                                           tags$label(id="bandF-label","Band F limit =", `for` = "bandF"),
                                           numericInput("bandF", NULL, 223000)),
                                       
                                       div(class = "input-group-council",
                                           tags$label(id="bandG-label","Band G limit =", `for` = "bandH"),
                                           numericInput("bandG", NULL, 324000)),
                                       
                                       div(id = "bandH-container",
                                           div(class = "input-group-council",
                                               tags$label("Band H limit", `for` = "bandH"),
                                               numericInput("bandH", NULL, 424000))),
                                       
                              )
                       ),
                       column(6,
                              #div(style = "height: 60px;", p("")), # Empty placeholder
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandArate"),
                                  sliderInput("bandArate", NULL, min = 1, max = 100, value = 5.4, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandBrate"),
                                  sliderInput("bandBrate", NULL, min = 1, max = 100, value = 10.1, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandCrate"),
                                  sliderInput("bandCrate", NULL, min = 1, max = 100, value = 20.4, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandDrate"),
                                  sliderInput("bandDrate", NULL, min = 1, max = 100, value = 30.0, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandErate"),
                                  sliderInput("bandErate", NULL, min = 1, max = 100, value = 50.4, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandFrate"),
                                  sliderInput("bandFrate", NULL, min = 1, max = 100, value = 60.7, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandGrate"),
                                  sliderInput("bandGrate", NULL, min = 1, max = 100, value = 75.4, step = 0.1)),
                              
                              div(class = "council-slider",
                                  tags$label("% top band:", `for` = "bandHrate"),
                                  sliderInput("bandHrate", NULL, min = 1, max = 100, value = 80.4, step = 0.1)),
                              
                              div(id="bandI-container",
                                  div(class = "council-slider",
                                      tags$label("% top band:", `for` = "bandIrate"),
                                      sliderInput("bandIrate", NULL, min = 1, max = 100, value = 80.4, step = 0.1))),
                       )
                     ),
                     #pie chart for the council tax, unformatted at present
                     plotlyOutput("pieChartCouncil"),
                     
            ),
            ###################################
            #Other taxes tab                  #  
            ###################################
            tabPanel(textOutput("other_taxes_tab_label"), value = "councilTax", h4("Info regarding other taxes detailed here"),
                     #empty gap first:
                     #div(style = "height: 20px;", p("")),
                     div(style = "height: 20px;", p("")),
                     radioButtons("otherTaxCountry", "Other taxes system", 
                                  choices = c("current", "scottish", "fully devolved"), 
                                  selected = "current", 
                                  inline = FALSE,
                                  width = "400px"),
                     div(style = "height: 20px;", p("")),
                     tags$div(style = "font-size: 17px;",textOutput("other_taxes_title")),
                     
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     
                     #Start with NDR:
                     div(class = "input-group-council",
                         tags$label("Property Tax = ", `for` = "property_tax_input"),
                         numericInput("property_tax_input", NULL, 0)),
                     #div(style = "height: 2px;", p("")),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("Land Transactional tax = ", `for` = "ltt_input"),
                         numericInput("ltt_input", NULL, 271)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("Landfill Disposals tax = ", `for` = "ldt_input"),
                         numericInput("ldt_input", NULL, 27)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("National Insurance = ", `for` = "ni_input"),
                         numericInput("ni_input", NULL, 6111)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("VAT = ", `for` = "vat_input"),
                         numericInput("vat_input", NULL, 9157)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("Corporation tax = ", `for` = "corporation_input"),
                         numericInput("corporation_input", NULL, 2698)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("Duties (fuel,alcohol etc) = ", `for` = "duties_input"),
                         numericInput("duties_input", NULL, 2500)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("Environmental Levy = ", `for` = "env_levy_input"),
                         numericInput("env_levy_input", NULL, 1300)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                     div(class = "input-group-council",
                         tags$label("Other = ", `for` = "other"),
                         numericInput("other", NULL, 2500)),
                     #br(),
                     tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
            )
            
          )
          
           
    ),
    column(7,
      fluidRow(
        #titlePanel(textOutput("piechart_title"))
        tags$div(style = "font-size: 18px;",textOutput("piechart_title"))
      ),    
      #Old and updated pie charts for the top right section of the screen:
      fluidRow(
      column(width = 6,
             align = "right",
             plotlyOutput("old_tax_piechart"),
      ),
      column(width = 6,
             align = "right",
             plotlyOutput("updated_tax_piechart"),
      )
      ),
      fluidRow(
        div(style = "height: 21px;", p("")),
      div(style = "display: flex; justify-content: center;",
          tableOutput("tax_table")
      )
      )
    )
    ),

    
)

########################
########################
########################
#Server Function:
########################
########################
########################
server <- function(input, output, session) {
  
  #Observe function to change Income Tax values depending on the choice
  observe({
    enabled_ids <- character(0)
    disabled_ids <- character(0)
    
    if (input$income_tax_system_choice == "Current Settlement") {
      enabled_ids <- c("BR", "HR", "AR")
      disabled_ids <- c("SR", "IR", "SRthreshold", "IRthreshold")
      updateNumericInput(session, "BRthreshold", value = 37500)
      updateNumericInput(session, "HRthreshold", value = 112500)
      updateSliderInput(session, "HR", min = 0, max = 1, value = 0.40,step = 0.01)
      updateSliderInput(session, "AR", min = 0, max = 1, value = 0.45,step = 0.01)
      
      #sliderInput("AR", NULL, min = 0, max = 1, value = 0.46, step = 0.01))
      
    } else if (input$income_tax_system_choice == "Scottish Model" || input$income_tax_system_choice == "Full Devolution") {
      enabled_ids <- c("SR", "BR", "IR", "HR", "AR", "SRthreshold", "IRthreshold")
      disabled_ids <- character(0)
      updateNumericInput(session, "BRthreshold", value = 11000)
      updateNumericInput(session, "HRthreshold", value = 107000)
      updateSliderInput(session, "BR", min = 0, max = 1, value = 0.2,step = 0.01)
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
  
  #Observations for Council Tax to enable selections if specific items are chosen.
  observe({
    if (input$councilTaxCountry == "current") {
      show("bandH-container")
      show("bandI-container")
    } else {
      hide("bandH-container")
      hide("bandI-container")
    }
  })
  
 

  #obersvations to change the council tax bands in response to what country choice:
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
  
  
  

  #Initialising the values for the income tax page to show previous and new amounts
  latest_value <- reactiveVal(0)
  saved_value <- reactiveVal(0)
  
  
  #Function to calculate the income tax for either scottish, current of fully deveolved scenarios:
  calculate_income_tax <- function(){
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
    if (input$income_tax_system_choice == "Scottish Model" || input$income_tax_system_choice == "Full Devolution"){
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
      
      starter_total <- sum((TIDist$SRtax) * TIDist$N, na.rm = TRUE)
      basic_total <- sum((TIDist$BRtax) * TIDist$N, na.rm = TRUE)
      inter_total <- sum((TIDist$IRtax) * TIDist$N, na.rm = TRUE)
      higher_total <- sum((TIDist$HRtax) * TIDist$N, na.rm = TRUE)
      additional_total <- sum((TIDist$ARtax) * TIDist$N, na.rm = TRUE)
      
      # Calculate total income tax payable
      TIDist$TotalTax <- (TIDist$SRtax + TIDist$BRtax + TIDist$IRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
      total_income_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)

      #reactively update the tax values for graphs:
      reactive_income_tax_data <- reactive({
        data.frame(
          tax_type = c(text_resources[[values$language]]$starter,text_resources[[values$language]]$basic, text_resources[[values$language]]$intermediate,text_resources[[values$language]]$higher, text_resources[[values$language]]$additional),
          count = c(starter_total, basic_total,inter_total, higher_total, additional_total)
        )
      })
      
      #barchart data:
      num_rows <- nrow(TIDist)
      counter <- 1
      band_sum <- 0
      results <-numeric(13)
      num_people <-numeric(13)
      band_people <- 0
      basic_list <-numeric(13)
      higher_list <- numeric(13)
      additional_list <- numeric(13)
      #scotland only
      starter_list <- numeric(13)
      inter_list <- numeric(13)
      basic_sum <- 0
      higher_sum <- 0
      addional_sum <- 0
      starter_sum <- 0
      inter_sum <- 0
      
      #loop through tax bands to populate the variables:
      for (i in 1:num_rows){
        #all bands except the last one:
        if (TIDist$TaxableIncome[i] <= (counter * 10000)){
          band_sum <- band_sum + (TIDist$TotalTax[i])
          band_people <- band_people + (TIDist$N[i])
          basic_sum <- basic_sum + (TIDist$BRtax[i] * TIDist$N[i])
          higher_sum <- higher_sum + (TIDist$HRtax[i] * TIDist$N[i])
          addional_sum <- addional_sum + (TIDist$ARtax[i] * TIDist$N[i])
          starter_sum <- starter_sum + (TIDist$SRtax[i] * TIDist$N[i])
          inter_sum <- inter_sum + (TIDist$IRtax[i] * TIDist$N[i])
          
        } else {
          #last band calculations:
          if (counter < 13){
            results[counter] <- band_sum
            num_people[counter] <- band_people
            basic_list[counter] <- basic_sum
            higher_list[counter] <- higher_sum
            additional_list[counter] <- addional_sum
            starter_list[counter] <- starter_sum
            inter_list[counter] <- inter_sum
            
            counter <- counter + 1
            
            band_sum <- TIDist$TotalTax[i]
            band_people <- TIDist$N[i]
            basic_sum <- (TIDist$BRtax[i] * TIDist$N[i])
            higher_sum <- (TIDist$HRtax[i] * TIDist$N[i])
            addional_sum <- (TIDist$ARtax[i] * TIDist$N[i])
            #scotland:
            starter_sum <- (TIDist$SRtax[i] * TIDist$N[i])
            inter_sum <- (TIDist$IRtax[i] * TIDist$N[i])
 
          }
          #last band save:
          else{
            band_sum <- band_sum + (TIDist$TotalTax[i])
            band_people <- band_people + (TIDist$N[i])
            basic_sum <- basic_sum + (TIDist$BRtax[i] * TIDist$N[i])
            higher_sum <- higher_sum + (TIDist$HRtax[i] * TIDist$N[i])
            addional_sum <- addional_sum + (TIDist$ARtax[i] * TIDist$N[i])
            starter_sum <- starter_sum + (TIDist$SRtax[i] * TIDist$N[i])
            inter_sum <- inter_sum + (TIDist$IRtax[i] * TIDist$N[i])
            
            if (i == num_rows){
              results[counter] <- band_sum
              num_people[counter] <- band_people
              basic_list[counter] <- basic_sum
              higher_list[counter] <- higher_sum
              additional_list[counter] <- addional_sum
              #Scotland Only:
              starter_list[counter] <- starter_sum
              inter_list[counter] <- inter_sum
            }
          }
        }
      }
      #divide by number of people per band if button Pressed:
      divisor <- if (values$divide) num_people else 1
      #barchart:
      bar_data <- reactive({
        vector1 <- c(starter_list/divisor)
        vector2 <- c(basic_list/divisor)
        vector3 <- c(inter_list/divisor)
        vector4 <- c(higher_list/divisor)
        vector5 <- c(additional_list/divisor)
        
        labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120+")
        legend_vector = c(text_resources[[values$language]]$starter,text_resources[[values$language]]$basic, text_resources[[values$language]]$intermediate,text_resources[[values$language]]$higher, text_resources[[values$language]]$additional)
        
        if (values$show_sum) {
          list(
            stacked = rbind(vector1, vector2, vector3,vector4, vector5),
            labels = labels,
            legend_vector = legend_vector
          )
        } else {
          # Calculate sum of vectors
          summed <- vector1 + vector2 + vector3 + vector4 + vector5
          list(
            stacked = summed,
            labels = labels
          )
          
        }
      })
      

    } else if (input$income_tax_system_choice == "Current Settlement"){
      #Similar to above, but less bands to worry about.
      
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
      
      basic_total <- sum((TIDist$BRtax) * TIDist$N, na.rm = TRUE)
      higher_total <- sum((TIDist$HRtax) * TIDist$N, na.rm = TRUE)
      additional_total <- sum((TIDist$ARtax) * TIDist$N, na.rm = TRUE)
      
      # Calculate total income tax payable
      TIDist$TotalTax <- ( TIDist$BRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
      total_income_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)

      #reactive tax values for pie chart:
      reactive_income_tax_data <- reactive({
        data.frame(
          tax_type = c(text_resources[[values$language]]$basic, text_resources[[values$language]]$higher, text_resources[[values$language]]$additional),
          count = c( basic_total, higher_total, additional_total)
        )
      })
      
      
      #Logic for barchart:
      num_rows <- nrow(TIDist)
      counter <- 1
      band_sum <- 0
      results <-numeric(13)
      num_people <-numeric(13)
      band_people <- 0
      basic_list <-numeric(13)
      higher_list <- numeric(13)
      additional_list <- numeric(13)
      basic_sum <- 0
      higher_sum <- 0
      addional_sum <- 0
      
      #Loop through the number of bands thats been decided upon:
      for (i in 1:num_rows){
        #All salary bands bar the last one
        if (TIDist$TaxableIncome[i] <= (counter * 10000)){
          band_sum <- band_sum + (TIDist$TotalTax[i])
          band_people <- band_people + (TIDist$N[i])
          basic_sum <- basic_sum + (TIDist$BRtax[i] * TIDist$N[i])
          higher_sum <- higher_sum + (TIDist$HRtax[i] * TIDist$N[i])
          addional_sum <- addional_sum + (TIDist$ARtax[i] * TIDist$N[i])
          
        } else {
          #last band tax calcs:
          if (counter < 13){
            results[counter] <- band_sum
            num_people[counter] <- band_people
            basic_list[counter] <- basic_sum
            higher_list[counter] <- higher_sum
            additional_list[counter] <- addional_sum
            
            counter <- counter + 1
            
            band_sum <- TIDist$TotalTax[i]
            band_people <- TIDist$N[i]
            basic_sum <- (TIDist$BRtax[i] * TIDist$N[i])
            higher_sum <- (TIDist$HRtax[i] * TIDist$N[i])
            addional_sum <- (TIDist$ARtax[i] * TIDist$N[i])
            
            
          }
          else{
            #Last band tax sums:
            band_sum <- band_sum + (TIDist$TotalTax[i])
            band_people <- band_people + (TIDist$N[i])
            basic_sum <- basic_sum + (TIDist$BRtax[i] * TIDist$N[i])
            higher_sum <- higher_sum + (TIDist$HRtax[i] * TIDist$N[i])
            addional_sum <- addional_sum + (TIDist$ARtax[i] * TIDist$N[i])
            
            if (i == num_rows){
              results[counter] <- band_sum
              num_people[counter] <- band_people
              basic_list[counter] <- basic_sum
              higher_list[counter] <- higher_sum
              additional_list[counter] <- addional_sum
            }
          }
          
        }
      }
      
      #if the divide by people has been pressed:
      divisor <- if (values$divide) num_people else 1
      
      #barchart:
      bar_data <- reactive({
        vector1 <- c(basic_list/divisor)
        vector2 <- c(higher_list/divisor)
        vector3 <- c(additional_list/divisor)
        labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120+")
        #legend_vector <- c("basic", "higher", "additional")
        legend_vector <- c(text_resources[[values$language]]$basic,text_resources[[values$language]]$higher, text_resources[[values$language]]$additional)
        
        #data prep for barchart:
        if (values$show_sum) {
          list(
            stacked = rbind(vector1, vector2, vector3),
            labels = labels,
            legend_vector = legend_vector
          )
        } else {
          # Calculate sum of vectors
          summed <- vector1 + vector2 + vector3
          list(
            stacked = summed,
            labels = labels
          )
        }
      })
      
    }
    
    #piechart output:
    output$pieChart <- renderPlotly({
      income_tax_pie_data <- reactive_income_tax_data()

      plot_ly(income_tax_pie_data, labels = ~tax_type, values = ~count, type = 'pie') %>%
        layout(
          title = text_resources[[values$language]]$income_tax_pie,
          margin = list(l = 20, r = 20, b = 10, t = 30),  # Adjust margins
          paper_bgcolor = 'white',  # Background color of the plot area
          plot_bgcolor = 'white'  # Background color of the chart area
          #width = 200px
        )
    })
    
    # Generate Stacked Bar Chart
    output$stackedPlot <- renderPlotly({
      data <- bar_data()
      
      # Ensure x-axis categories are factors with the correct order
      x_categories <- factor(data$labels, levels = data$labels)

      if (values$show_sum){
        p <- plot_ly(
          x = x_categories,
          y = ~data$stacked[1,],
          type = 'bar',
          name = ~data$legend_vector[1],
          marker = list(color = 'rgba(255, 99, 132, 0.6)')
        ) %>%
          add_trace(
            y = ~data$stacked[2,],
            name = ~data$legend_vector[2],
            marker = list(color = 'rgba(54, 162, 235, 0.6)')
          ) %>%
          add_trace(
            y = ~data$stacked[3,],
            name = ~data$legend_vector[3],
            marker = list(color = 'rgba(75, 192, 192, 0.6)')
          )
        
        #Current tax button pressed:
        if (input$income_tax_system_choice != "Current Settlement" && nrow(data$stacked) > 3) {
          p <- p %>%
            add_trace(
              y = ~data$stacked[4,],
              name = ~data$legend_vector[4],
              marker = list(color = 'rgba(223, 192, 192, 0.6)')
            ) %>%
            add_trace(
              y = ~data$stacked[5,],
              name = ~data$legend_vector[5],
              marker = list(color = 'rgba(23, 192, 192, 0.6)')
            )
        }
        p <- p %>%
          layout(
            barmode = 'stack',
            title = text_resources[[values$language]]$income_stacked_graph_title,
            xaxis = list(title = text_resources[[values$language]]$income_bar_x),
            yaxis = list(title = text_resources[[values$language]]$income_bar_y)
          )
        
        p
      }else {
        # Plot summed values
        plot_ly(
          x = x_categories,
          y = ~data$stacked,
          type = 'bar',
          name = 'Sum',
          marker = list(color = 'rgba(255, 99, 132, 0.6)')
        ) %>%
          layout(
            barmode = 'group',
            title = text_resources[[values$language]]$income_stacked_graph_title,
            xaxis = list(title = text_resources[[values$language]]$income_bar_x),
            yaxis = list(title = text_resources[[values$language]]$income_bar_y)
          )
        
      }
    })
    
    #return total_income_tax_sum for the total_income_tax total
    total_income_tax_sum
  }
  
  
  #piechart:
  output$old_tax_piechart <- renderPlotly({
    old_tax_data <- data.frame(
      labels = c("Income", "Council","NDR","LTT","LDT","NI","VAT","Corporation","Duties","Env Levy", "Other"),
      count = c(3322,2716,1100,271,27,6111,9157,2698,2500,1300,2500)
      
    )
    # Calculate total sum
    total_count <- sum(old_tax_data$count)
    
    # Add percentage column calculated manually
    old_tax_data$percentage <- round((old_tax_data$count / total_count) * 100, 2)
    
    plot_ly(old_tax_data, values = ~count, type = 'pie',
            textinfo = 'percent', 
            hoverinfo = 'percent',
            texttemplate = ~paste0(labels, ": ", percentage, "%"),
            automargin = TRUE) %>%
      layout(
        title = list(
          text = text_resources[[values$language]]$previous_tax_piechart,  # Set the title text
          font = list(size = 15)
          #x = 10,                      # Align title to the left
          #xanchor = 'left'            # Set the anchor point to left
        ),
        #title = "",
        margin = list(l = 0, r = 10, b = 20, t = 40),  # Adjust margins
        paper_bgcolor = 'white',  # Background color of the plot area
        plot_bgcolor = 'white',  # Background color of the chart area
        showlegend = FALSE
        #width = 200px
      )
      
    })
  

  
  
  output$totalTaxOutput <- renderText({
    total_income_tax_sum <- calculate_income_tax()
    latest_value(total_income_tax_sum)
    paste(text_resources[[values$language]]$total_income_title_1, "\n", text_resources[[values$language]]$total_income_title_2, round(total_income_tax_sum/1000000000, digits = 2), " billion")
  })
  
  output$newTotalTaxOutput <- renderText({
    newAmount <- saved_value()
    paste(text_resources[[values$language]]$previous_income_title, newAmount, "(", round(newAmount/1000000000, digits = 2), " billion)")
  })
  
  
  
  #Function to calculate the reactive council tax from user inputs.
  calculate_council_tax <- function(){
    
    #top band value:
    topBandValue <- input$topBandValue
    
    #Band inputs:
    bandAlimit <- input$bandA
    bandBlimit <- input$bandB
    bandClimit <- input$bandC
    bandDlimit <- input$bandD
    bandElimit <- input$bandE
    bandFlimit <- input$bandF
    bandGlimit <- input$bandG
    bandHlimit <- input$bandH
    
    #rate inputs:
    bandArate <- input$bandArate
    bandBrate <- input$bandBrate
    bandCrate <- input$bandCrate
    bandDrate <- input$bandDrate
    bandErate <- input$bandErate
    bandFrate <- input$bandFrate
    bandGrate <- input$bandGrate
    bandHrate <- input$bandHrate
    
    
    
    CTdata <- read.csv("councilTaxEnglandAndWales.csv", sep=",")
    bandAtax <- CTdata$N[1] * topBandValue * (bandArate / 100)
    bandBtax <- CTdata$N[2] * topBandValue * (bandBrate / 100)
    bandCtax <- CTdata$N[3] * topBandValue * (bandCrate / 100)
    bandDtax <- CTdata$N[4] * topBandValue * (bandDrate / 100)
    bandEtax <- CTdata$N[5] * topBandValue * (bandErate / 100)
    bandFtax <- CTdata$N[6] * topBandValue * (bandFrate / 100)
    bandGtax <- CTdata$N[7] * topBandValue * (bandGrate / 100)
    bandHtax <- CTdata$N[8] * topBandValue * (bandHrate / 100)
    
    
    
    
    
    if (input$councilTaxCountry == "Wales"){
      bandIrate <- input$bandIrate
      #print("hello")
      bandItax <- CTdata$N[9] * topBandValue  * (bandIrate / 100)
      
      councilTax <- bandAtax + bandBtax + bandCtax + bandDtax + bandEtax + bandFtax + bandFtax + bandGtax + bandHtax+ bandItax
      reactive_council_values <- reactive({

        data.frame(
          band_label = c("A", "B","C","D", "E","F","G","H","I"),
          #count = c(TIDist$SRtax * TIDist$N, TIDist$BRtax * TIDist$N, TIDist$IRtax * TIDist$N, TIDist$HRtax * TIDist$N, TIDist$ARtax * TIDist$N)
          band = c(bandAtax, bandBtax,bandCtax, bandDtax, bandEtax,bandFtax,bandGtax, bandHtax,bandItax)
          #band = c(1,2,3,4,5,6,7,8,9)
        )
        
      })
    }else{
      councilTax <- bandAtax + bandBtax + bandCtax + bandDtax + bandEtax + bandFtax + bandFtax + bandGtax + bandHtax
      
      reactive_council_values <- reactive({
        
        data.frame(
          band_label = c("A", "B","C","D", "E","F","G","H"),
          #count = c(TIDist$SRtax * TIDist$N, TIDist$BRtax * TIDist$N, TIDist$IRtax * TIDist$N, TIDist$HRtax * TIDist$N, TIDist$ARtax * TIDist$N)
          band = c(bandAtax, bandBtax,bandCtax, bandDtax, bandEtax,bandFtax,bandGtax, bandHtax)
          #band = c(1,2,3,4,5,6,7,8)
        )
        
      })
    }
    
    #plotPychart:
    #piechart:
    output$pieChartCouncil <- renderPlotly({
      reactive_council_values <- reactive_council_values()
      
      plot_ly(reactive_council_values, labels = ~band_label, values = ~band, type = 'pie') %>%
        layout(
          title = 'Council Tax income distribution',
          margin = list(l = 20, r = 20, b = 10, t = 35),  # Adjust margins
          paper_bgcolor = 'lightgray',  # Background color of the plot area
          plot_bgcolor = 'white'  # Background color of the chart area
          #width = 200px
        )
    })
    
    #return councilTax:
    councilTax
  }
  
  ###############
  ###############
  #Outputs for UI
  ###############
  ###############
  #outputs in the top menu (Previous and current tax returns):
  output$old_total_tax <- renderText({
    paste(text_resources[[values$language]]$old_total_tax, "= £3Billion")
  })
  
  output$updated_total_tax <- renderText({
    paste(text_resources[[values$language]]$updated_total_tax, "= £5Billion")
  })
  
  
  output$councilTaxOutput <- renderText({
    councilTax <- calculate_council_tax()
    paste("Council Tax return = ",councilTax/1000000000, "billion")
  })
 
  
  
  
  
  observeEvent(input$calculate, {
    # Save the current latest value when the button is clicked
    saved_value(latest_value())
  })
  
  #toggle button for incometax barchart:
  # Reactive value to keep track of button state
  values <- reactiveValues(
    divide = FALSE,
    show_sum = FALSE,
    language = "English",
    total_income_tax = NULL,
    total_council_tax = NULL
  )
  
  observe({
    # Assuming calculate_income_tax and calculate_council_tax functions are called
    values$total_income_tax <- calculate_income_tax()
    values$total_council_tax <- calculate_council_tax()
  })
  
  output$updated_tax_piechart <- renderPlotly({
    # Ensure totals are available
    if (is.null(values$total_income_tax) || is.null(values$total_council_tax)) {
      return(NULL)
    }
    
    #ndr data:
    total_ndr_tax <- (input$ndrTotal) * 1000000
    tourism_levy_tax <- (input$tourism_levy_total) * 1000000
    property_tax <- (input$property_tax_input) * 1000000
    ltt_tax <- (input$ltt_input) * 1000000
    ldt_tax <- (input$ldt_input) * 1000000
    ni_tax <- (input$ni_input) * 1000000
    vat_tax <- (input$vat_input) * 1000000
    
    
    # Data for pie chart
    pie_data <- data.frame(
      category = c("Income", "Council","NDR","Tourism","Property","LTT","LDT","NI","VAT"),
      amount = c(values$total_income_tax, values$total_council_tax,total_ndr_tax,tourism_levy_tax, property_tax, ltt_tax, ldt_tax, ni_tax, vat_tax)
    )
    
    updated_total_count <- sum(pie_data$amount)
    pie_data$percentage <- round((pie_data$amount / updated_total_count) * 100, 2)
    
    plot_ly(pie_data, values = ~amount, type = 'pie',
            textinfo = 'text', 
            hoverinfo = 'text',
            text = ~paste0(category, ": ", percentage, "%"),
            automargin = TRUE) %>%
      layout(
        title = list(
          text = text_resources[[values$language]]$updated_tax_piechart,
          font = list(size = 15)
          #x = 0,
          #xanchor = 'left'
        ),
        #title = "Updated Tax Breakdown:",
        margin = list(l = 0, r = 0, b = 20, t = 40),  # Adjust margins
        paper_bgcolor = 'white',  # Background color of the plot area 
        plot_bgcolor = 'white',  # Background color of the chart area
        showlegend = FALSE
        #width = 200px
      )
  })
  
  
  
  observeEvent(input$toggleButton, {
    values$divide <- !values$divide
    updateButtonLabels()  # Update button label when toggled
  })
  observeEvent(input$viewButton, {
    values$show_sum <- !values$show_sum
    updateButtonLabels()  # Update button label when toggled
  })
  
  updateButtonLabels <- function() {
    updateActionButton(session, "toggleButton", label = ifelse(values$divide, 
                                                               text_resources[[values$language]]$revert, 
                                                               text_resources[[values$language]]$divide_by_people))
    updateActionButton(session, "viewButton", label = ifelse(values$show_sum, 
                                                             text_resources[[values$language]]$revert, 
                                                             text_resources[[values$language]]$show_breakdown))
    updateActionButton(session, "translateButton", label = text_resources[[values$language]]$translate_button)
  }
  
  
  #Langauge and translation stuff:
  observeEvent(input$translateButton, {
    values$language <- ifelse(values$language == "English", "Welsh", "English")
    updateButtonLabels()
  })
  
  ###################
  #Table data       #
  ###################
  #backup for table stuff:
    # Define data values and row names
  static_data <- reactive({    
    tax <- c(
      text_resources[[values$language]]$devolved_taxes,
      text_resources[[values$language]]$income_tax_devolved,
      text_resources[[values$language]]$council_tax,
      text_resources[[values$language]]$ndr,
      text_resources[[values$language]]$property_tax,
      text_resources[[values$language]]$ltt,
      text_resources[[values$language]]$ldt,
      text_resources[[values$language]]$tourism_levy,
      NA,
      text_resources[[values$language]]$non_devolved_taxes,
      text_resources[[values$language]]$income_tax,
      text_resources[[values$language]]$ni,
      text_resources[[values$language]]$vat,
      text_resources[[values$language]]$corporation_tax,
      text_resources[[values$language]]$duties,
      text_resources[[values$language]]$env_levy,
      text_resources[[values$language]]$other,
      NA,
    
      text_resources[[values$language]]$block_grant,
      text_resources[[values$language]]$tot_welsh_taxes,
      text_resources[[values$language]]$welsh_budget
    )
    
    c_estimates <- c(NA, 3322, 2716, 1100, 0, 271, 27, 0, NA, NA, 5315, 6111, 9157, 2698, 2500, 1300, 2500,NA,NA,NA,NA)
    
    updated_estimates <- c(NA, 3322, round((values$total_council_tax)/1000000), input$ndrTotal, 0, input$ltt_input, input$ldt_input, 0, NA, NA, round((values$total_income_tax)/1000000), 6111, 9157, 2698, 2500, 1300, 2500,NA,NA,NA,NA)

    
    c_estimate_sum <- c(
      NA, NA, NA, NA, NA, NA, NA, NA,  # Values for devolved taxes
      sum(c(3322, 2716, 1100, 0, 271, 27, 0)),  # Total devolved taxes
      NA, NA, NA, NA, NA, NA, NA, NA,  # Values for non-devolved taxes
      sum(c(5315, 6111, 9157, 2698, 2500, 1300, 2500)),
      19000,37017,26436
      # Total non-devolved taxes
    )
    blank_col <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    
    # Create a data frame
    data_frame <- data.frame(
      Tax = tax,
      `Current Estimates` = c_estimates,
      est_tot= c_estimate_sum,
      blank_col = blank_col,
      updated_new = updated_estimates,
      upd_tot = c_estimate_sum,
      stringsAsFactors = FALSE  # Ensure strings are not converted to factors
      
    )
    
  })
  
  
    output$tax_table <- renderTable({
      combined_data <- static_data()
      # Convert NA values to empty strings for better display
      combined_data[is.na(combined_data)] <- ""
      combined_data
    }, rownames = FALSE)  # Do not display row names
  
    
    
  ######################################
  #Observe income Tax model selection: #
  ######################################
    
    observe({
      if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")) {
        shinyjs::enable("num_rows")
      } else {
        shinyjs::disable("num_rows")
      }
    })
    
  #Observe event to ensure that if current settlement is selcted, the other rows go back:
  observeEvent(input$income_tax_system_choice, {
    if (input$income_tax_system_choice == "Current Settlement"){
      updateNumericInput(session, "num_rows", value = 3)
    }
  })
  
  
  ###################################################################
  #Observe model selection to toggle the Uk rate to 10/20/30% or 0% #
  ###################################################################
  uk_rate_text_1 <- reactive({
    if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")){
      "UK Rate: 0%"
    } else {
      "UK Rate: 10%"
    }
  })
  uk_rate_text_2 <- reactive({
    if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")){
      "UK Rate: 0%"
    } else {
      "UK Rate: 30%"
    }
  })
  uk_rate_text_3 <- reactive({
    if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")){
      "UK Rate: 0%"
    } else {
      "UK Rate: 35%"
    }
  })
  
  output$uk_rate_text_1 <- renderText({ uk_rate_text_1() })
  output$uk_rate_text_2 <- renderText({ uk_rate_text_2() })
  output$uk_rate_text_3 <- renderText({ uk_rate_text_3() })
  
  #####################################
  #Dynamic Rows for income tax input: #
  #####################################
  
    output$dynamic_rows <- renderUI({
      num_rows <- input$num_rows
      
      # Create an empty list to store dynamic rows
      rows <- list()
      
      # Only generate additional rows if num_rows is greater than 3
      if (num_rows > 3) {
        # Loop to create dynamic rows starting from the 4th row
        for (i in 4:num_rows) {
          rows[[i - 3]] <- fluidRow(
            column(4,
                   div(class = "rates_input_left",
                       tags$label(paste("Rate", i, "Threshold:"), `for` = paste0("rate_", i, "_t")),
                       numericInput(paste0("rate_", i, "_t"), NULL, 120000, step = 100))
            ),
            column(2,
                   div(style = "height: 12px;", p("")),
                   div(class = "grey-text-box", 
                       "UK Rate: 0%"
                   )
            ),
            column(6,
                   div(class = "rates_input_right_slider custom-numeric-input",
                       tags$label(paste("Welsh rate", i, ":"), `for` = paste0("welsh_rate_", i)),
                       sliderInput(paste0("welsh_rate_", i), NULL, min = 0, max = 100, value = 20, step = 1))
            )
          )
        }
      }
      
      # Return the list of dynamic rows
      do.call(tagList, rows)
    })
  
  
  ###############################
  #New Income Tax calculations: #
  ###############################
  
  calculate_income_tax_new <- function(){
    PA <- input$PAnew
    PAlimit <- input$PAlimit
    
    #need to know how many bands there are to be able to calulcate the income tax:
    #thresholds <- c(37500, 112500)   # Define thresholds for all bands
    #rates <- c(0.2, 0.4, 0.45)
    num_rows <- input$num_rows
    
    thresholds <- numeric(num_rows -1)
    rates <- numeric(num_rows)
    
    
    
    for (i in 1:num_rows){
      rate_name <- paste0("welsh_rate_",i)
      rates[i] <- input[[rate_name]]
      if (i == num_rows){
        break
      }else{
        threshold_name <- paste0("rate_",i,"_t")
        thresholds[i] <- input[[threshold_name]]
      }
    }
    
    #thresholds <- c(37500, 112500)   # Define thresholds for all bands
    #rates <- c(0.2, 0.4, 0.45)
    
    # Import taxable income distribution
    TIDist_new <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")
    
    # Calculate Personal Allowance
    TIDist_new$PA <- ifelse(TIDist_new$TaxableIncome <= PAlimit, PA, pmax(0, PA - 0.5 * (TIDist_new$TaxableIncome - PAlimit)))
    
    # Calculate Total Taxable Income after allowance
    TIDist_new$totTaxableInc <- pmax(TIDist_new$TaxableIncome - TIDist_new$PA, 0)
    
    # Initialize variables for tax calculations
    TIDist_new$r1_income <- pmin(TIDist_new$totTaxableInc, thresholds[1]) # First band
    TIDist_new$r1_tax <- rates[1] * TIDist_new$r1_income
    remaining_income <- TIDist_new$totTaxableInc - TIDist_new$r1_income
    
    # Loop through the remaining bands
    for (i in 2:length(thresholds)) {
      # Calculate the income for the current band
      band_income <- pmin(remaining_income, thresholds[i])  # Income within the current threshold
      TIDist_new[[paste0("r", i, "_income")]] <- band_income                       # Assign to dynamic column name
      TIDist_new[[paste0("r", i, "_tax")]] <- rates[i] * band_income               # Calculate tax for current band
      remaining_income <- remaining_income - band_income                       # Update remaining income
    }
    
    # Calculate tax for income above the highest threshold
    TIDist_new[[paste0("r", length(thresholds) + 1, "_income")]] <- pmax(remaining_income, 0)
    TIDist_new[[paste0("r", length(thresholds) + 1, "_tax")]] <- rates[length(rates)] * TIDist_new[[paste0("r", length(thresholds) + 1, "_income")]]
    
    # Calculate total tax payable
    TIDist_new$TotalTax <- rowSums(TIDist_new[grep("_tax$", names(TIDist_new))], na.rm = TRUE) * TIDist_new$N
    total_income_tax_new <- sum(TIDist_new$TotalTax, na.rm = TRUE)
    
    total_income_tax_new

  }
  
  boop <- function(){
    test <- 10
    test
  }
  
  #get this new value to the main program:
  output$new_income_tax <- renderText({
    new_total_income <- calculate_income_tax_new()
    #paste(text_resources[[values$language]]$total_income_title_1, "\n", text_resources[[values$language]]$total_income_title_2, round(total_income_tax_sum/1000000000, digits = 2), " billion")
    paste("new value = ", new_total_income)  
    })
  
  

  ###################
  #Translation data:#
  ###################
  #translate button:
  output$translate_button <- renderText({
    text_resources[[values$language]]$translate_button
  })
  #Main App title:
  output$title <- renderText({
    text_resources[[values$language]]$title
  })
  
  
  #Income Tax Page intro:
  output$income_tax_intro <- renderText({
    text_resources[[values$language]]$income_tax_intro
  })
  
  #Inputs for income tax page: pa_box
  output$pa_box <- renderText({
    text_resources[[values$language]]$pa_box
  })
  output$pa_limit <- renderText({
    text_resources[[values$language]]$pa_limit
  })
  output$sr_threshold <- renderText({
    text_resources[[values$language]]$sr_threshold
  })
  output$br_threshold <- renderText({
    text_resources[[values$language]]$br_threshold
  })
  output$ir_threshold <- renderText({
    text_resources[[values$language]]$ir_threshold
  })
  output$hr_threshold <- renderText({
    text_resources[[values$language]]$hr_threshold
  })
  output$sr <- renderText({
    text_resources[[values$language]]$sr
  })
  output$br <- renderText({
    text_resources[[values$language]]$br
  })
  output$ir <- renderText({
    text_resources[[values$language]]$ir
  })
  output$hr <- renderText({
    text_resources[[values$language]]$hr
  })
  output$ar <- renderText({
    text_resources[[values$language]]$ar
  })
  
  #Select income tax type:
  output$select_income_system <- renderText({
    text_resources[[values$language]]$select_income_system
  })
  #Buttons for income tax charts and graphs:
  output$divide_by_people <- renderText({
    text_resources[[values$language]]$divide_by_people
  })
  output$divide_button <- renderText({
    text_resources[[values$language]]$divide_button
  })
  output$show_breakdown <- renderText({
    text_resources[[values$language]]$show_breakdown
  })
  output$update_with_new_filters <- renderText({
    text_resources[[values$language]]$update_with_new_filters
  })
  
  
  output$income_tax <- renderText({
    text_resources[[values$language]]$income_tax
  })
  
  
  output$income_band_x <- renderText({
    text_resources[[values$language]]$income_band_x
  })
  
  output$income_band_y <- renderText({
    text_resources[[values$language]]$income_band_y
  })
  output$total_income_title <- renderText({
    text_resources[[values$language]]$total_income_title
  })
  output$previous_income_title <- renderText({
    text_resources[[values$language]]$previous_income_title
  })
  
  
  #Tabs:
  output$council_tax <- renderText({
    text_resources[[values$language]]$council_tax
  })
  output$ndr <- renderText({
    text_resources[[values$language]]$ndr
  })
  output$ltt <- renderText({
    text_resources[[values$language]]$ltt
  })
  output$ldt <- renderText({
    text_resources[[values$language]]$ldt
  })
  output$local_taxes_tab_label <- renderText({
    text_resources[[values$language]]$local_taxes_tab_label
  })
  output$other_taxes_tab_label <- renderText({
    text_resources[[values$language]]$other_taxes_tab_label
  })
  
  #Income Tax_location choice:
  output$current <- renderText({
    text_resources[[values$language]]$current
  })
  output$fully_devolved <- renderText({
    text_resources[[values$language]]$fully_devolved
  })
  output$scottish <- renderText({
    text_resources[[values$language]]$scottish
  })
  
  output$see_more_button <- renderText({
    text_resources[[values$language]]$see_more_button
  })
  output$contact_us_button <- renderText({
    text_resources[[values$language]]$contact_us_button
  })
  output$main_app_intro <- renderText({
    text_resources[[values$language]]$main_app_intro
  })
  output$local_taxes_tab_intro <- renderText({
    text_resources[[values$language]]$local_taxes_tab_intro
  })
  output$local_tax_system_selection <- renderText({
    text_resources[[values$language]]$local_tax_system_selection
  })
  output$rate <- renderText({
    text_resources[[values$language]]$rate
  })
  output$ndr_input <- renderText({
    text_resources[[values$language]]$ndr_input
  })
  output$tourism_input <- renderText({
    text_resources[[values$language]]$tourism_input
  })
  output$block_grant_input <- renderText({
    text_resources[[values$language]]$block_grant_input
  })
  output$council_tax_title <- renderText({
    text_resources[[values$language]]$council_tax_title
  })
  output$other_taxes_title <- renderText({
    text_resources[[values$language]]$other_taxes_title
  })
  output$piechart_title <- renderText({
    text_resources[[values$language]]$piechart_title
  })
}

###########################################
###########################################
#run the app:                             #
###########################################
###########################################
shinyApp(ui = ui, server = server)