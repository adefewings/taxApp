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
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
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
             tabPanel(textOutput("income_tax"), value = "incomeTab",
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
                               #column(6,
                               #        div(style = "height: 20px;", p("")),
                               #        div(class = "output1-container",
                               #            verbatimTextOutput("totalTaxOutput")
                               #        )
                               #)
                               ),
                               #adding the new requested rows and columns for the inputs:
                               
                               
                               numericInput("num_rows", "Number of Tax Bands", value = 3, min = 3, max = 10),
                               fluidRow(
                                 column(5,
                                        div(class = "pa_input_left",
                                            tags$label("Personal Allowance:", `for` = "pa_new"),
                                            numericInput("pa_new", NULL, 12500, step = 100)),
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
                                            numericInput("rate_1_t", NULL, 37500, step = 100)),
                                        
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
                                            numericInput("rate_2_t", NULL, 112500, step = 100)),
                                        
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
                               tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                               
                               fluidRow(
                                 
                                 plotlyOutput("income_tax_piechart"),
                                 div(class = "calculate-button-container",
                                     actionButton("toggleButton", label = textOutput("divide_by_people")),
                                     actionButton("viewButton", label = textOutput("show_breakdown"))
                                 ),
                                 
                               ), 
                               
                               div(class = "output1-container",
                                   verbatimTextOutput("new_income_tax")
                               ),
                               tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                               
                               
                               plotlyOutput("stacked_plot_income_tax"),
                               
                               div(style = "height: 10px;", p("")), # Empty placeholder
                               tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                               
                               
                               
                               
                      ),
             ),
             ###################################
             #Local taxes tab                  #  
             ###################################
             tabPanel(textOutput("local_taxes_tab_label"), value = "councilTax",
                      tags$div(style = "font-size: 16px;",textOutput("local_taxes_tab_intro")),
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      
                      div(style = "height: 20px;", p("")),
                      
                      fluidRow(
                        column(4,
                               div(class = "switch-label",
                                   switchInput(inputId = "toggle", label = "Non-Domestic Rates", value = TRUE)
                               ),
                               div(class = "switch-label",
                                   switchInput(inputId = "toggle", label = "Council tax", value = TRUE)
                               ),
                               div(class = "switch-label",
                                   switchInput(inputId = "toggle", label = "Property tax", value = TRUE)
                               ),
                               div(class = "switch-label",
                                   switchInput(inputId = "toggle", label = "Tourist Levy", value = TRUE)
                               ),
                        )
                      ),
                      
                      
                      #empty gap first:
                      div(style = "height: 20px;", p("")),
                      
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      
                      fluidRow(column(4,
                                      
                                      
                      ),
                      column(4,
                             
                      )),
                      
                      #Start with NDR:
                      
                      tags$div(style = "font-size: 16px;","Non-Domestic rates:"),
                      div(class = "local_taxes_numeric",
                          tags$label("Multiplier", `for` = "ndr_muliplier"),
                          numericInput("ndr_muliplier", NULL, 0.562, step = 0.001)),
                      fluidRow(
                        column(5,
                               div(class = "local_taxes-text-box", 
                                   "Small Business Rates Relief:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Empty / Partly Occupied Premises Relief"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Charitable Relief"
                               ),
                        ),
                        column(4,
                               
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "small_business_relief", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "empty_relief", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "charitable_relief", value = TRUE)
                               ),
                               
                        )
                      ),
                      
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      tags$div(style = "font-size: 16px;","Council Tax:"),
                      div(class = "local_taxes_numeric",
                          tags$label("Average Band D rate:", `for` = "av_band_d_rate"),
                          numericInput("av_band_d_rate", NULL, 2024, step = 1)),
                      fluidRow(
                        column(5,
                               div(class = "local_taxes-text-box", 
                                   "Council Tax Reduction Scheme:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Single Person Discount:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Disability related discounts:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Student Exemption:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Armed Forces Exemption:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Care leaver Exemption:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Vacant Property Exemptions:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Other Exemptions:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Second Home Premium:"
                               ),
                               div(class = "local_taxes-text-box", 
                                   "Empty Property Premium:"
                               ),
                               
                               
                        ),
                        column(4,
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "council_tax_reduction_scheme", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "single_person_ex", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "disability_discount", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "student_ex", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "armed_forces_ex", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "care_leaver_ex", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "vacant_ex", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "other_ex", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "second_home_prem", value = TRUE)
                               ),
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "empty_prem", value = TRUE)
                               ),
                        )
                        ,),
                      
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      tags$div(style = "font-size: 16px;","Property Tax:"),
                      fluidRow(column(5,
                                      div(class = "local_taxes-text-box", 
                                          "Residential Properties:"
                                      ), 
                      ),
                      column(4,
                             div(class = "local_taxes-toggle",
                                 switchInput(inputId = "residential_properties", value = TRUE)
                             ),
                      )
                      ),
                      fluidRow(
                        column(1,
                               div(style = "height: 20px;", p("")),
                        ),
                        column(6,
                               div(class = "property_tax_numeric",
                                   tags$label("Tax Free Allowance:", `for` = "tax_free_allowance"),
                                   numericInput("tax_free_allowance", NULL, 0, step = 1)),
                               div(class = "property_tax_numeric",
                                   tags$label("Tax Rate on land component:", `for` = "residential_land"),
                                   numericInput("residential_land", NULL, 0, step = 1)),
                               div(class = "property_tax_numeric",
                                   tags$label("Tax Rate on Buildings component:", `for` = "residential_building"),
                                   numericInput("residential_building", NULL, 0, step = 1)),
                        ),
                      ),
                      
                      fluidRow(column(5,
                                      div(class = "local_taxes-text-box", 
                                          "Non-domestic Properties:"
                                      ), 
                      ),
                      column(4,
                             div(class = "local_taxes-toggle",
                                 switchInput(inputId = "non_residential_properties", value = TRUE)
                             ),
                      )
                      ),
                      fluidRow(
                        column(1,
                               div(style = "height: 20px;", p("")),
                        ),
                        column(6,
                               
                               div(class = "property_tax_numeric",
                                   tags$label("Tax Rate on land component:", `for` = "non_residential_land"),
                                   numericInput("non_residential_land", NULL, 0, step = 1)),
                               div(class = "property_tax_numeric",
                                   tags$label("Tax Rate on Buildings component:", `for` = "non_residential_building"),
                                   numericInput("non_residential_building", NULL, 0, step = 1)),
                        ),
                      ),
                      
                      
                      
                      
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      tags$div(style = "font-size: 16px;","Tourism Levy (TBC)"),
                      br(),
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      #titlePanel(textOutput("council_tax_title")),
                      
                      
                      
                      
             ),
             ###################################
             #Other taxes tab                  #  
             ###################################
             tabPanel(textOutput("other_taxes_tab_label"), value = "councilTax",
                      tags$div(style = "font-size: 16px;","Other taxes intro..........."),
                      div(style = "height: 20px;", p("")),
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      tags$div(style = "font-size: 16px;","Land Transactional Tax:"),
                      div(style = "height: 10px;", p("")),
                      fluidRow(
                        column(5,
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 1 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate1_th"),
                                   numericInput("ltt_rate1_th", NULL, 180000, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 2 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate2_th"),
                                   numericInput("ltt_rate2_th", NULL, 180000, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 3 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate3_th"),
                                   numericInput("ltt_rate3_th", NULL, 180000, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 4 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate4_th"),
                                   numericInput("ltt_rate4_th", NULL, 180000, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 5 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate5_th"),
                                   numericInput("ltt_rate5_th", NULL, 180000, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 6 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate6_th"),
                                   numericInput("ltt_rate6_th", NULL, 180000, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 7 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate7_th"),
                                   numericInput("ltt_rate7_th", NULL, 180000, step = 100)),
                               
                        ),
                        column(7,
                               fluidRow(
                                 column(6,
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 1:", `for` = "ltt_main_rate1"),
                                            numericInput("ltt_main_rate1", NULL, 0, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 2:", `for` = "ltt_main_rate2"),
                                            numericInput("ltt_main_rate2", NULL, 0, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 3:", `for` = "ltt_main_rate3"),
                                            numericInput("ltt_main_rate3", NULL, 6, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 4:", `for` = "ltt_main_rate4"),
                                            numericInput("ltt_main_rate4", NULL, 6, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 5:", `for` = "ltt_main_rate5"),
                                            numericInput("ltt_main_rate5", NULL, 7.5, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 6:", `for` = "ltt_main_rate6"),
                                            numericInput("ltt_main_rate6", NULL, 10, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Main Rate 7:", `for` = "ltt_main_rate7"),
                                            numericInput("ltt_main_rate7", NULL, 12.5, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                 ), 
                                 
                                 column(6,
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 1:", `for` = "ltt_higher_rate1"),
                                            numericInput("ltt_higher_rate1", NULL, 4, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 2:", `for` = "ltt_higher_rate2"),
                                            numericInput("ltt_higher_rate2", NULL, 7.5, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 3:", `for` = "ltt_higher_rate3"),
                                            numericInput("ltt_higher_rate3", NULL, 7.5, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 4:", `for` = "ltt_higher_rate4"),
                                            numericInput("ltt_higher_rate4", NULL, 9, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 5:", `for` = "ltt_higher_rate5"),
                                            numericInput("ltt_higher_rate5", NULL, 11.5, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 6:", `for` = "ltt_higher_rate6"),
                                            numericInput("ltt_higher_rate6", NULL, 14, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        div(class = "ltt_input_middle",
                                            tags$label("Higher rate 7:", `for` = "ltt_higher_rate7"),
                                            numericInput("ltt_higher_rate7", NULL, 16, step = 0.1),
                                            tags$span("%", style = "margin-left: 5px;")),
                                        
                                 ),
                               ),
                        ),
                        
                        
                        
                      ),
                      
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      tags$div(style = "font-size: 16px;","Landfill Disposals Tax:"),
                      div(style = "height: 10px;", p("")),
                      div(class = "local_taxes_numeric",
                          tags$label("Standard rate per tonne:", `for` = "ldt_std_rate"),
                          numericInput("ldt_std_rate", NULL, 103.7, step = 0.01)),
                      div(class = "local_taxes_numeric",
                          tags$label("Lower Rate per tonne:", `for` = "ldt_lower_rate"),
                          numericInput("ldt_lower_rate", NULL, 3.30, step = 0.01)),
                      div(class = "local_taxes_numeric",
                          tags$label("Unauthorised Disposals rate per tonne:", `for` = "ldt_unauth_rate"),
                          numericInput("ldt_unauth_rate", NULL, 155.55, step = 0.01)),
                      
                      
                      #empty gap first:
                      #div(style = "height: 20px;", p("")),
                      div(style = "height: 80px;", p("")),
                      
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
             tags$div(style = "font-size: 18px; text-align: center;",textOutput("piechart_title"))
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