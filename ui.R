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
    column(6,
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
                      tags$div(style = "font-size: 16px;",textOutput("income_tax_intro")),
                               div(style = "height: 20px;", p("")), # Empty placeholder
                               tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                               ##Start with NDR:
                               #div(class = "input-group-block custom-numeric-input",
                               #   tags$label(textOutput("block_grant_input"), `for` = "block_grant_total"),
                               #  numericInput("block_grant_total", NULL, 19000, step = 100)),
                               
                               # tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                               
                               fluidRow(column(6,
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
                               
                               
                               numericInput("num_rows", "Number of Tax Bands:", value = 3, min = 1, max = 10),
                               fluidRow(
                                 column(5,
                                        div(class = "pa_input_left",
                                            tags$label("Personal Allowance:", `for` = "pa_new"),
                                            numericInput("pa_new", NULL, 12500, step = 100)),
                                 ),
                                 
                                 column(4,
                                        div(style = "height: 20px;", p(""))
                                 ),
                                 
                                 column(3,
                                        div(class = "pa_input_right",
                                            tags$label("PA Limit:", `for` = "pa_limit"),
                                            numericInput("pa_limit", NULL, 100000, step = 100)),
                                        
                                 ),
                               ),
                               
                               
                              
                              

                               uiOutput("dynamic_rows"),
                               tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                               
                              fluidRow(
                                actionButton("show_income_tax_figures","See Figures:"),
                                
                                conditionalPanel(
                                  condition = "input.show_income_tax_figures % 2 == 1",
                                  plotlyOutput("income_tax_piechart"),
                                  tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                                  
                                  div(class = "calculate-button-container",
                                      actionButton("toggleButton", label = textOutput("divide_by_people")),
                                      actionButton("viewButton", label = textOutput("show_breakdown")),
                                      ),
                                      #div(class = "output1-container",
                                       #   verbatimTextOutput("new_income_tax")
                                      #),
                                      
                                      
                                      plotlyOutput("stacked_plot_income_tax"),
                                      
                                      div(style = "height: 10px;", p("")), # Empty placeholder
                                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                                  ),
                                  
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
                                   switchInput(inputId = "ndr_toggle", label = "Non-Domestic Rates", value = TRUE)
                               ),
                               div(class = "switch-label",
                                   switchInput(inputId = "council_toggle", label = "Council tax", value = TRUE)
                               ),
                               
                        ),
                        column(4,
                               div(class = "switch-label",
                                   switchInput(inputId = "property_toggle", label = "Property tax", value = FALSE)
                               ),
                               div(class = "switch-label",
                                   switchInput(inputId = "tourism_toggle", label = "Tourist Levy", value = FALSE)
                               ),
                               )
                      ),
                      
                      
                      #empty gap first:
                      div(style = "height: 20px;", p("")),
                      
                      #tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                      
                      
                      
                      #Start with NDR:
                      
                      
                      conditionalPanel(
                        condition = "input.ndr_toggle == true",
                        tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                        
                        tags$div(style = "font-size: 16px;","Non-Domestic rates:"),
                        div(class = "local_taxes_numeric",
                            tags$label("Multiplier", `for` = "ndr_muliplier"),
                            numericInput("ndr_muliplier", NULL, 0.562, step = 0.001)),
                        fluidRow(
                          column(4,
                                 div(class = "local_taxes_text_box", 
                                     "Small Business Rates Relief:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Empty / Partly Occupied Premises Relief"
                                 ),
                                 div(class = "local_taxes_text_box", 
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
                                 div(style = "height: 15px;", p("")),
                                 div(class = "local_taxes-toggle",
                                     switchInput(inputId = "charitable_relief", value = TRUE)
                                 ),
                                 
                          )
                        ),
                        
                      ),
                      
                      
                      conditionalPanel(
                        condition = "input.council_toggle == true",
                        tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                        tags$div(style = "font-size: 16px;","Council Tax:"),
                        div(class = "local_taxes_numeric",
                            tags$label("Average Band D rate:", `for` = "av_band_d_rate"),
                            numericInput("av_band_d_rate", NULL, 2024, step = 1)),
                        fluidRow(
                          column(4,
                                 div(class = "local_taxes_text_box", 
                                     "Council Tax Reduction Scheme:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Single Person Discount:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Disability related discounts:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Student Exemption:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Armed Forces Exemption:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Care leaver Exemption:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Vacant Property Exemptions:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Other Exemptions:"
                                 ),
                                 div(class = "local_taxes_text_box", 
                                     "Second Home Premium:"
                                 ),
                                 div(class = "local_taxes_text_box", 
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
                      ),

                      conditionalPanel(
                        condition = "input.property_toggle == true",
                        
                        tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                        tags$div(style = "font-size: 16px;","Property Tax:"),
                        fluidRow(column(5,
                                        div(class = "local_taxes-text-box", 
                                            "Residential Properties:"
                                        ), 
                        ),
                        column(4,
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "residential_properties", value = FALSE)
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
                                     numericInput("tax_free_allowance", NULL, 0, step = 1000, min = 0)),
                                 div(class = "property_tax_numeric",
                                     tags$label("Tax Rate on land component:", `for` = "residential_land"),
                                     numericInput("residential_land", NULL, 0, step = 0.1)),
                                 div(class = "property_tax_numeric",
                                     tags$label("Tax Rate on Buildings component:", `for` = "residential_building"),
                                     numericInput("residential_building", NULL, 0, step = 0.1)),
                          ),
                        ),
                        
                        fluidRow(column(5,
                                        div(class = "local_taxes-text-box", 
                                            "Non-domestic Properties:"
                                        ), 
                        ),
                        column(4,
                               div(class = "local_taxes-toggle",
                                   switchInput(inputId = "non_residential_properties", value = FALSE)
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
                                     numericInput("non_residential_land", NULL, 0, step = 0.1)),
                                 div(class = "property_tax_numeric",
                                     tags$label("Tax Rate on Buildings component:", `for` = "non_residential_building"),
                                     numericInput("non_residential_building", NULL, 0, step = 0.1)),
                          ),
                        ),
                        
                      ),
                        
                      
                      conditionalPanel(
                        condition = "input.tourism_toggle == true",
                        tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
                        tags$div(style = "font-size: 16px;","Tourism Levy (TBC)"),
                        br(),
                      ),
                      
                      
                      
                      #tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
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
                        column(4,
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 1 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate1_th"),
                                   numericInput("ltt_rate1_th", NULL, 50270, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 2 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate2_th"),
                                   numericInput("ltt_rate2_th", NULL, 137710, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 3 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate3_th"),
                                   numericInput("ltt_rate3_th", NULL, 137710, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 4 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate4_th"),
                                   numericInput("ltt_rate4_th", NULL, 137710, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 5 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate5_th"),
                                   numericInput("ltt_rate5_th", NULL, 137710, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 6 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate6_th"),
                                   numericInput("ltt_rate6_th", NULL, 137710, step = 100)),
                               div(class = "ltt_input_left",
                                   tags$label(HTML("Rate 7 Threshold:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;£"), `for` = "ltt_rate7_th"),
                                   numericInput("ltt_rate7_th", NULL, 0, step = 100)),
                               
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
                      
                      
                      #empty gap:
                      div(style = "height: 40px;", p("")),
                      tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),

                      #tags$div(style = "background-color: #C50031; height: 2px; width: 100%; margin: 10px 0; padding: 0;"),
             )
             
           )
           
           
    ),
    
    column(6,
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