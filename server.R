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
    
    
    
    
    if (input$income_tax_system_choice == "Current Settlement" || input$income_tax_system_choice == "Scottish Model") {
      #enabled_ids <- c("BR", "HR", "AR")
      disabled_ids <- c("pa_new", "pa_limit")
      #updateNumericInput(session, "BRthreshold", value = 37500)
      #updateNumericInput(session, "HRthreshold", value = 112500)
      #updateSliderInput(session, "HR", min = 0, max = 1, value = 0.40,step = 0.01)
      #updateSliderInput(session, "AR", min = 0, max = 1, value = 0.45,step = 0.01)
      
      #sliderInput("AR", NULL, min = 0, max = 1, value = 0.46, step = 0.01))
      
    } else if (input$income_tax_system_choice == "Full Devolution") {
      enabled_ids <- c("pa_new", "pa_limit")
      disabled_ids <- character(0)
      #updateNumericInput(session, "BRthreshold", value = 11000)
      #updateNumericInput(session, "HRthreshold", value = 107000)
      #updateSliderInput(session, "BR", min = 0, max = 1, value = 0.2,step = 0.01)
      #updateSliderInput(session, "HR", min = 0, max = 1, value = 0.41,step = 0.01)
      #updateSliderInput(session, "AR", min = 0, max = 1, value = 0.46,step = 0.01)
      
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
  
  
  
  
  
  #Initialising the values for the income tax page to show previous and new amounts
  latest_value <- reactiveVal(0)
  
  
  
  
  
  
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
    #total_income_tax_sum <- calculate_income_tax_new()
    total_income_tax_sum <- 100000000
    latest_value(total_income_tax_sum)
    paste(text_resources[[values$language]]$total_income_title_1, "\n", text_resources[[values$language]]$total_income_title_2, round(total_income_tax_sum/1000000000, digits = 2), " billion")
  })
  
  
  
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
    values$total_income_tax <- 100000000
    values$total_council_tax <- 10000000000
  })
  
  output$updated_tax_piechart <- renderPlotly({
    # Ensure totals are available
    if (is.null(values$total_income_tax) || is.null(values$total_council_tax)) {
      return(NULL)
    }
    
    #ndr data:
    total_ndr_tax <- 1000 * 1000000
    tourism_levy_tax <- 1000 * 1000000
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
    
    updated_estimates <- c(NA, 3322, round((values$total_council_tax)/1000000), 1000, 0, input$ltt_input, input$ldt_input, 0, NA, NA, round((values$total_income_tax)/1000000), 6111, 9157, 2698, 2500, 1300, 2500,NA,NA,NA,NA)
    
    
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
      Millions = tax,
      current_estimates = c_estimates,
      est_tot= c_estimate_sum,
      blank_col = blank_col,
      updated_new = updated_estimates,
      upd_tot = c_estimate_sum,
      stringsAsFactors = FALSE  # Ensure strings are not converted to factors
      
    )
    
  })
  
  
  output$tax_table <- renderTable({
    combined_data <- static_data()
    colnames(combined_data)[colnames(combined_data) == "Millions"] <- "£Millions"
    colnames(combined_data)[colnames(combined_data) == "current_estimates"] <- "Current Estimates"
    colnames(combined_data)[colnames(combined_data) == "blank_col"] <- "               "
    colnames(combined_data)[colnames(combined_data) == "updated_new"] <- "With your changes implemented"
    colnames(combined_data)[colnames(combined_data) == "est_tot"] <- "Current Total:"
    colnames(combined_data)[colnames(combined_data) == "upd_tot"] <- "Updated Total:"
    
    
    
    # Convert NA values to empty strings for better display
    combined_data[is.na(combined_data)] <- ""
    combined_data
  }, 
  rownames = FALSE,
  align = 'lrrrrr'
  )  # Do not display row names
  
  
  
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
    PA <- input$pa_new
    PAlimit <- input$pa_limit
    
    #need to know how many bands there are to be able to calulcate the income tax:
    #thresholds <- c(37500, 112500)   # Define thresholds for all bands
    #rates <- c(0.2, 0.4, 0.45)
    num_rows <- input$num_rows
    
    thresholds <- numeric(num_rows -1)
    rates <- numeric(num_rows)
    
    
    
    for (i in 1:num_rows){
      rate_name <- paste0("welsh_rate_",i)
      rates[i] <- input[[rate_name]] / 100
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
    
    #Vector containing breakdown of the tax types for piechart:
    tax_columns <- grep("_tax$", names(TIDist_new), value = TRUE)  # Find column names ending with '_tax'
    tax_totals <- colSums(TIDist_new[tax_columns] * TIDist_new$N, na.rm = TRUE)    # Calculate column sums for each tax type
    
    
    
    #data for the piechart
    reactive_income_tax_data_new <- reactive({
      data.frame(
        tax_type = tax_columns,
        count = tax_totals
      )
    })
    
    
    output$income_tax_piechart <- renderPlotly({
      income_tax_pie_data <- reactive_income_tax_data_new()
      
      plot_ly(income_tax_pie_data, labels = ~tax_type, values = ~count, type = 'pie') %>%
        layout(
          title = text_resources[[values$language]]$income_tax_pie,
          margin = list(l = 20, r = 20, b = 10, t = 30),  # Adjust margins
          paper_bgcolor = 'white',  # Background color of the plot area
          plot_bgcolor = 'white'  # Background color of the chart area
          #width = 200px
        )
    })
    
    #######################################
    #Bar chart stuff:                     #
    #######################################
    
    num_rows_in_data <- nrow(TIDist_new)
    counter <- 1
    band_sum <- 0
    results <-numeric(13)
    num_people <-numeric(13)
    band_people <- 0
    
    total_rates <- num_rows
    rates_list <- vector("list", total_rates)  # Create a list with num_rows slots
    sum_list <- vector("list",total_rates)
    
    
    # Populate each sublist with numeric(13)
    for (i in 1:total_rates) {
      rates_list[[i]] <- numeric(13)
      sum_list[[i]] <- 0
    }
    
    #loop through tax bands to populate the variables:
    for (i in 1:num_rows_in_data){
      #all bands except the last one:
      if (TIDist_new$TaxableIncome[i] <= (counter * 10000)){
        band_sum <- band_sum + (TIDist_new$TotalTax[i])
        band_people <- band_people + (TIDist_new$N[i])
        
        for (j in 1:total_rates){
          tax_type <- paste0("r",j,"_tax")
          sum_list[[j]] <- sum_list[[j]] + (TIDist_new[[tax_type]][i] * TIDist_new$N[i])
          
        }
        
        
      } else {
        #last band calculations:
        if (counter < 13){
          results[counter] <- band_sum
          num_people[counter] <- band_people
          
          for (j in 1:total_rates){
            
            rates_list[[j]][counter] <- sum_list[[j]]
          }
          
          
          counter <- counter + 1
          
          band_sum <- TIDist_new$TotalTax[i]
          band_people <- TIDist_new$N[i]
          
          for (j in 1:total_rates){
            tax_type <- paste0("r",j,"_tax")
            sum_list[[j]] <- (TIDist_new[[tax_type]][i] * TIDist_new$N[i])
          }
          
        }
        #last band save:
        else{
          band_sum <- band_sum + (TIDist_new$TotalTax[i])
          band_people <- band_people + (TIDist_new$N[i])
          
          for (j in 1:total_rates){
            tax_type <- paste0("r",j,"_tax")
            sum_list[[j]] <- sum_list[[j]] + (TIDist_new[[tax_type]][i] * TIDist_new$N[i])
          }
          
          
          if (i == num_rows_in_data){
            results[counter] <- band_sum
            num_people[counter] <- band_people
            
            for (j in 1:total_rates){
              rates_list[[j]][counter] <- sum_list[[j]]
            }
            
          }
        }
      }
    }
    #print(rates_list)
    
    
    #divide by number of people per band if button Pressed:
    divisor <- if (values$divide) num_people else 1
    #barchart:
    bar_data <- reactive({
      
      
      #new list to hold the divided by people:
      rates_divided_list <- vector("list", total_rates)
      for (i in 1:total_rates) {
        rates_divided_list[[i]] <- numeric(13)
      }
      
      #now to work out the new values:
      for (j in 1:total_rates){
        rates_divided_list[[j]] = c(rates_list[[j]]/divisor)
        
      }
      #print(rates)
      
      #vector1 <- c(starter_list/divisor)
      #vector2 <- c(basic_list/divisor)
      #vector3 <- c(inter_list/divisor)
      #vector4 <- c(higher_list/divisor)
      #vector5 <- c(additional_list/divisor)
      
      labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120+")
      #legend_vector = c(text_resources[[values$language]]$starter,text_resources[[values$language]]$basic, text_resources[[values$language]]$intermediate,text_resources[[values$language]]$higher, text_resources[[values$language]]$additional)
      
      if (values$show_sum) {
        list(
          #stacked = rbind(vector1, vector2, vector3,vector4, vector5),
          
          stacked = do.call(rbind,rates_divided_list),
          labels = labels,
          legend_vector = tax_columns
        )
      } else {
        # Calculate sum of vectors
        #summed <- vector1 + vector2 + vector3 + vector4 + vector5
        summed <- Reduce(`+`,rates_divided_list)
        #print(summed)
        list(
          #stacked = summed,
          stacked = summed,
          labels = labels
        )
        
      }
    })
    
    
    # Generate Stacked Bar Chart
    output$stacked_plot_income_tax <- renderPlotly({
      data <- bar_data()
      
      # Ensure x-axis categories are factors with the correct order
      x_categories <- factor(data$labels, levels = data$labels)
      
      if (values$show_sum){
        
        colors <- c('rgba(255, 99, 132, 0.6)',  # First color
                    'rgba(54, 162, 235, 0.6)',  # Second color
                    'rgba(75, 192, 192, 0.6)',  # Third color
                    'rgba(223, 192, 192, 0.6)', # Fourth color
                    'rgba(23, 192, 192, 0.6)',  # Fifth color
                    'rgba(192, 192, 75, 0.6)')  # Add more colors if necessary
        p <- plot_ly(
          x = x_categories,
          y = ~data$stacked[1,],
          type = 'bar',
          name = ~data$legend_vector[1],
          marker = list(color = 'rgba(255, 99, 132, 0.6)')
        ) 
        total_rates <- input$num_rows
        for (i in 2:total_rates){
          p <- p %>%
            add_trace(
              y = data$stacked[i,],
              name = data$legend_vector[i],
              marker = list(color = colors[i])
            )
        }
        
        
        # p <- p %>%
        #  add_trace(
        #   y = data$stacked[3,],
        #  name = data$legend_vector[3],
        # marker = list(color = 'rgba(23, 192, 192, 0.6)')
        #)
        
        
        
        
        #for (i in 2:total_rates) {
        #  color <- switch(i,
        #                 'rgba(54, 162, 235, 0.6)',  # Color for the second trace
        #                'rgba(75, 192, 192, 0.6)',  # Color for the third trace
        #               'rgba(223, 192, 192, 0.6)', # Color for the fourth trace
        #              'rgba(23, 192, 192, 0.6)',  # Color for the fifth trace
        #             'rgba(192, 192, 75, 0.6)')  # Color for additional traces if needed
        
        #p <- p %>%
        # add_trace(
        #  y = ~data$stacked[i,],
        # name = ~data$legend_vector[i],
        #  marker = list(color = color)
        #)
        
        #}
        
        
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
    
    
    total_income_tax_new
    
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