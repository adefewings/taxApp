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
      disabled_ids <- c("pa_new", "pa_limit")
    } else if (input$income_tax_system_choice == "Scottish Model"){
      disabled_ids <- c("pa_new", "pa_limit")
    } else if (input$income_tax_system_choice == "Full Devolution"){
      enabled_ids <- c("pa_new", "pa_limit" )
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
  
  observeEvent(input$show_income_tax_figures, {
    if (input$show_income_tax_figures %% 2 == 1) {
      updateActionButton(session, "show_income_tax_figures", label = "Hide figures")
    } else {
      updateActionButton(session, "show_income_tax_figures", label = "See figures")
    }
  })
  
  
  
  #piechart:
  output$old_tax_piechart <- renderPlotly({
    old_tax_data <- data.frame(
      labels = c("Block Grant","Income", "Council","NDR","Property","LDT","LTT","Tourism Levy"),
      count = c(app_parameters_list$current_blockgrant,app_parameters_list$current_income_tax_dev + app_parameters_list$current_income_tax_nondev,app_parameters_list$current_council,app_parameters_list$current_ndr,app_parameters_list$current_property,app_parameters_list$current_ldt,app_parameters_list$current_ltt,app_parameters_list$current_tourism)
    )
    
    # Calculate total sum
    total_count <- sum(old_tax_data$count)
    
    # Add percentage column calculated manually
    old_tax_data$percentage <- round((old_tax_data$count / total_count) * 100, 2)
    segment_colors <- c("#3498DB", "#2ECC71", "#F1C40F", "#E67E22", 
                        "#E74C3C", "#9B59B6", "#1ABC9C", "#FF6F61")
    
    # Modify plot_ly to hide labels for segments < 5%
    plot_ly(old_tax_data, values = ~count, type = 'pie',
            text = ~paste0(labels, ": ", percentage, "%"),
            textinfo = 'percent',
            hoverinfo = 'text',  # Show hoverinfo for all segments
            texttemplate = ~ifelse(percentage >= 5, paste0(labels, ": ", percentage, "%"), ""),  # Show labels only if percentage >= 5%
            textposition = 'inside',
            marker = list(colors = segment_colors),
            automargin = TRUE) %>%
      layout(
        title = list(
          text = text_resources[[values$language]]$previous_tax_piechart,  # Set the title text
          font = list(size = 15)
        ),
        height = 350,
        width = 350,
        margin = list(l = 0, r = 10, b = 20, t = 40),  # Adjust margins
        paper_bgcolor = 'white',  # Background color of the plot area
        plot_bgcolor = 'white',  # Background color of the chart area 
        showlegend = FALSE
      )
  })
  
  #sum of the old tax data:
  
  output$current_total_sum <- renderText({
    total <- c(app_parameters_list$current_blockgrant,app_parameters_list$current_income_tax_dev + app_parameters_list$current_income_tax_nondev,app_parameters_list$current_council,app_parameters_list$current_ndr,app_parameters_list$current_property,app_parameters_list$current_ldt,app_parameters_list$current_ltt,app_parameters_list$current_tourism)
    #formatted_difference <- format(abs(difference_rounded), big.mark = ",", scientific = FALSE)
    total_formatted = format(sum(total), big.mark = ",", scientific = FALSE)
    return(paste0("Total = £",total_formatted, " million"))
  })
  plot_data <- reactive({
    # Function call for incomeTax:
    income_tax_totals <- calculate_income_tax_new()
    total_income_tax = income_tax_totals$non_devolved_total + income_tax_totals$devolved_total
    
    # Data for pie chart
    pie_data <- data.frame(
      category = c("Block Grant", "Income", "Council", "NDR", "Property", "LDT", "LTT", "Tourism Levy"),
      amount = c(app_parameters_list$current_blockgrant, total_income_tax / 1000000, 
                 calculate_council_tax_new() / 1000000, calculate_ndr_tax_new() / 1000000,
                 (calculate_property_new() / 1000000), round(calculate_ldt_tax_new() / 1000000), 
                 calculate_ltt_tax_new() / 1000000, round(calculate_tourism_tax_new() / 1000000))
    )
    
    pie_data$category <- factor(pie_data$category, levels = c("Block Grant", "Income", "Council", "NDR", "Property", "LDT", "LTT", "Tourism Levy"))
    
    
    values$updated_total_tax_sum = round(sum(pie_data$amount))
    
    updated_total_count <- sum(pie_data$amount)
    pie_data$percentage <- round((pie_data$amount / updated_total_count) * 100, 2)
    
    values$dynamic_radius_variable = ((sum(pie_data$amount) - 30261) / 30261)
    
    dynamic_radius <- values$dynamic_radius_variable
    print(dynamic_radius)
    
    # Define colors for each segment
    segment_colors <- c("#3498DB", "#2ECC71", "#F1C40F", "#E67E22", 
                        "#E74C3C", "#9B59B6", "#1ABC9C", "#FF6F61")
    
    plot <- plot_ly(pie_data, values = ~amount, type = 'pie',
                    textinfo = 'label+percent', 
                    hoverinfo = 'text',
                    text = ~paste0(category, ": ", percentage, "%"),
                    texttemplate = ~ifelse(percentage >= 5, paste0(category, ": ", percentage, "%"), ""),
                    textposition = 'inside', 
                    marker = list(colors = segment_colors), 
                    automargin = TRUE,
                    sort = FALSE) %>%
      layout(
        title = list(
          text = text_resources[[values$language]]$updated_tax_piechart,
          font = list(size = 15)
        ),
        height = min(450, 350 * (1 + dynamic_radius)),  # Increased height
        width = min(450, 350 * (1 + dynamic_radius)),   # Increased width
        margin = list(l = 0, r = 10, b = 20, t = 40),  # Increased top margin
        paper_bgcolor = 'white', 
        plot_bgcolor = 'white', 
        showlegend = FALSE
      )
    
    return(plot)
  })
  
  output$updated_tax_piechart <- renderPlotly({
    plot_data()  # Call the reactive expression
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("tax_pie_chart", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create a temporary HTML file to save the plot
      temp_html_file <- tempfile(fileext = ".html")
      temp_png_file <- tempfile(fileext = ".png")
      
      # Save the plot as an HTML file
      htmlwidgets::saveWidget(as_widget(plot_data()), temp_html_file, selfcontained = TRUE)
      
      
      # Read the content of the HTML file
      html_content <- readLines(temp_html_file)
      
      # Wrap the plot in a div with CSS to shift it down and to the right
      css_style <- "<style>div#piechart-wrapper { position: relative; top: 50px; left: 50px; }</style>"
      div_wrapper_start <- "<div id='piechart-wrapper'>"
      div_wrapper_end <- "</div>"
      
      # Add the CSS and wrap the plot in a div
      html_content <- c(css_style, div_wrapper_start, html_content, div_wrapper_end)
      
      # Write the modified HTML back to the file
      writeLines(html_content, temp_html_file)
      
      
      # Use webshot to capture the HTML file as a PNG
      webshot(temp_html_file, file = temp_png_file, vwidth = 500, vheight = 500)
      
      # Load the logo and the plot
      logo <- image_read("businessLogo.png")  # Ensure the logo is in the working directory
      plot_image <- image_read(temp_png_file)
      
      # Resize the logo for better positioning
      logo_resized <- image_scale(logo, "120x120")  # Adjust size to make it bigger
      
      # Get dimensions of the plot image
      plot_width <- image_info(plot_image)$width
      plot_height <- image_info(plot_image)$height
      
      # Calculate position for the logo in the top right corner
      logo_position_x <- plot_width - image_info(logo_resized)$width - 10  # 10 pixels padding from right
      logo_position_y <- 10  # 10 pixels padding from top
      
      # Composite the plot image and the logo
      combined_image <- image_composite(plot_image, logo_resized, offset = paste0("+", logo_position_x, "+", logo_position_y))
      
      # Save the final image with the logo
      image_write(combined_image, path = file)
    }
  )
  
  output$updated_total_sum <-renderText({
    updated_sum = values$updated_total_tax_sum
    updated_sum_formatted = format(sum(updated_sum), big.mark = ",", scientific = FALSE)
    
    return(paste0("Total = £",updated_sum_formatted, " million"))
    
  })
  
  ###############
  ###############
  #Outputs for UI
  ###############
  ###############
  #outputs in the top menu (Previous and current tax returns):


  
  #toggle button for incometax barchart:
  # Reactive value to keep track of button state
  values <- reactiveValues(
    divide = FALSE,
    show_sum = FALSE,
    language = "English",
    new_total_income_tax = 0,
    income_tax_difference_list = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    ndr_difference = 0,
    council_difference = 0,
    property_difference = 0,
    tourism_difference = 0,
    ltt_difference = 0,
    ldt_difference = 0,
    updated_total_tax_sum = 0,
    dynamic_radius_variable = 0
    
  )
  

  
  
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

  
  calculate_ndr_tax_new <- function(){
    
    if (input$ndr_toggle){
      
      ndr_value <- 1969500000
      
      multiplier <- input$ndr_muliplier
      small_business_relief <- input$small_business_relief
      empty_relief <- input$empty_relief
      charitable_relief <- input$charitable_relief
      
      
      ndr_value = ndr_value * multiplier
      
      # Conditionally subtract based on the toggle switches
      if (small_business_relief) {
        ndr_value <- ndr_value - 2500000
      }
      
      if (empty_relief) {
        ndr_value <- ndr_value - 3000000
      }
      
      if (charitable_relief) {
        ndr_value <- ndr_value - 1234550
      }
      
    }else{
      ndr_value <- 0
    }
    
    #update the ndr difference:
    values$ndr_difference = ndr_value - 1100000000
    
    return(ndr_value)
    
  }
  
  
  calculate_property_new <- function(){
    if (input$property_toggle){
      property_tax <- 0
      #need to calculate based on the values:
      
      if (input$residential_properties){
        property_tax <- property_tax + 500000000
      }
      
      if(input$non_residential_properties){
        property_tax <- property_tax + 500000000
      }
    }else{
      property_tax <- 0
    }
    #update property tax value:
    values$property_difference = property_tax - 0
    
    return(property_tax)
  }
  
  #calculate council tax dynamically
  calculate_council_tax_new <- function(){
    if (input$council_toggle){
      council_tax <- round(10000000000/3.11)
      
      #band d rate:
      av_band_d_rate <- input$av_band_d_rate
      #now for exemptions:
      council_tax_reduction_scheme <- input$council_tax_reduction_scheme
      single_person_ex <- input$single_person_ex
      disability_discount <- input$disability_discount
      student_ex <- input$student_ex
      armed_forces_ex <- input$armed_forces_ex
      care_leaver_ex <- input$care_leaver_ex
      vacant_ex <- input$vacant_ex
      other_ex <- input$other_ex
      second_home_prem <- input$second_home_prem
      empty_prem <- input$empty_prem
      
      if (council_tax_reduction_scheme){
        council_tax <- council_tax - 50000000
      }
      if (single_person_ex){
        council_tax <- council_tax - 50000000
      }
      if (disability_discount){
        council_tax <- council_tax - 50000000
      }
      if (student_ex){
        council_tax <- council_tax - 50000000
      }
      if (armed_forces_ex){
        council_tax <- council_tax - 50000000
      }
      if (care_leaver_ex){
        council_tax <- council_tax - 50000000
      }
      if (vacant_ex){
        council_tax <- council_tax - 50000000
      }
      if (other_ex){
        council_tax <- council_tax - 50000000
      }
      if (second_home_prem){
        council_tax <- council_tax - 50000000
      }
      if (empty_prem){
        council_tax <- council_tax - 50000000
      }
      
    } 
    else{
      council_tax <- 0
    }
    
    #update dynamic_difference
    values$council_difference = council_tax - 2715000000
    
    return(council_tax)
  }
  
  calculate_tourism_tax_new <- function(){
    if(input$tourism_toggle){
      tourism_tax <- 500000000
    }else{
      tourism_tax <- 0
    }
    
    return(tourism_tax)
  }
  
  calculate_ltt_tax_new <- function(){
    ltt_tax <- 17100000
    
    ltt_tax <- ltt_tax * input$ltt_higher_rate7
    
    #update ltt:
    values$ltt_difference = ltt_tax - 274000000
    
    return(ltt_tax)
  }
  
  
  calculate_ldt_tax_new <- function(){
    ldt_tax <- 260000
    ldt_tax <- ldt_tax * input$ldt_std_rate
    
    #update value for arrow in UI
    values$ldt_difference = ldt_tax - 27000000
    
  }
  
  # Define a reactive expression to calculate the income tax
  calculate_all_taxes <- reactive({
    req(input$pa_new, input$pa_limit, input$num_rows)  # Ensure necessary inputs are available
    
    # Calculate individual taxes
    total_income_tax_new <- calculate_income_tax_new()
    total_ndr <- calculate_ndr_tax_new()
    property_tax_updated <- calculate_property_new()
    council_tax_updated <- calculate_council_tax_new()
    tourism_tax_updated <- calculate_tourism_tax_new()
    #ltt_tax_upated <- calculate_ltt_tax_new()
    
    # Return a list or dataframe of all the calculated taxes
    taxes <- list(
      income_tax_devolved = total_income_tax_new$devolved_total,
      income_tax_non_devolved = total_income_tax_new$non_devolved_total,
      ndr_tax = total_ndr,
      property_tax = property_tax_updated,
      council_tax = council_tax_updated,
      tourism_tax = tourism_tax_updated,
      ltt_tax = calculate_ltt_tax_new(),
      ldt_tax = calculate_ldt_tax_new()
    )
    
    return(taxes)
  })
  
  
  
  
  
  app_parameters_data <- read.csv("appdata.csv", header = FALSE, stringsAsFactors = FALSE)
  app_parameters_list <- setNames(as.list(app_parameters_data$V2), app_parameters_data$V1)
  
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
    
    #call fucntion which updated taxes reactively:
    updated_taxes <- calculate_all_taxes()
    
    
    c_estimates <- c(NA, app_parameters_list$current_income_tax_dev, app_parameters_list$current_council, app_parameters_list$current_ndr, app_parameters_list$current_property, app_parameters_list$current_ltt, app_parameters_list$current_ldt, app_parameters_list$current_tourism, NA, NA, app_parameters_list$current_income_tax_nondev, app_parameters_list$current_ni, app_parameters_list$current_vat, app_parameters_list$current_corporation, app_parameters_list$current_duties, app_parameters_list$current_env_levy, app_parameters_list$current_other,NA,NA,NA,NA)
    
    updated_estimates <- c(NA, round((updated_taxes$income_tax_devolved)/1000000), round((updated_taxes$council_tax)/1000000), round((updated_taxes$ndr_tax)/1000000), round((updated_taxes$property_tax)/1000000), round(updated_taxes$ltt_tax)/1000000, round((updated_taxes$ldt_tax)/1000000), round((updated_taxes$tourism_tax)/1000000), NA, NA, round((updated_taxes$income_tax_non_devolved)/1000000), app_parameters_list$current_ni, app_parameters_list$current_vat, app_parameters_list$current_corporation, app_parameters_list$current_duties, app_parameters_list$current_env_levy, app_parameters_list$current_other,NA,NA,NA,NA)
    
    
    c_estimate_sum <- c(
      NA, NA, NA, NA, NA, NA, NA, NA,  # Values for devolved taxes
      sum(c_estimates[2:8]),  # Total devolved taxes
      NA, NA, NA, NA, NA, NA, NA, NA,  # Values for non-devolved taxes
      sum(c_estimates[11:17]),
      app_parameters_list$current_blockgrant,sum(c_estimates[2:8]) + sum(c_estimates[11:17]),sum(c_estimates[2:8]) + app_parameters_list$current_blockgrant
      # Total non-devolved taxes
    )
    
    c_updated_sum <- c(
      NA, NA, NA, NA, NA, NA, NA, NA,  # Values for devolved taxes
      sum(updated_estimates[2:8]),  # Total devolved taxes
      NA, NA, NA, NA, NA, NA, NA, NA,  # Values for non-devolved taxes
      sum(updated_estimates[11:17]),
      app_parameters_list$current_blockgrant,sum(updated_estimates[2:8]) + sum(updated_estimates[11:17]),sum(updated_estimates[2:8]) + app_parameters_list$current_blockgrant
      # Total non-devolved taxes
    )
    
    # Create a data frame
    data_frame <- data.frame(
      Millions = tax,
      current_estimates = c_estimates,
      est_tot= c_estimate_sum,
      updated_new = updated_estimates,
      upd_tot = c_updated_sum,
      stringsAsFactors = FALSE  # Ensure strings are not converted to factors
    )
    
  })
  
  
  output$tax_table1 <- renderUI({
    combined_data <- static_data()
    
    # Rename columns
    colnames(combined_data)[colnames(combined_data) == "Millions"] <- "£Millions"
    colnames(combined_data)[colnames(combined_data) == "current_estimates"] <- "Current Estimates"
    colnames(combined_data)[colnames(combined_data) == "updated_new"] <- "With your<br>changes implemented"
    colnames(combined_data)[colnames(combined_data) == "est_tot"] <- "Current Total:"
    colnames(combined_data)[colnames(combined_data) == "upd_tot"] <- "Updated Total:"
    
    # Handle NAs first: Replace NAs with empty strings
    combined_data[is.na(combined_data)] <- ""
    
    # Format numeric columns with commas (skip already blank cells)
    combined_data$`Current Estimates` <- ifelse(combined_data$`Current Estimates` == "", "", comma(as.numeric(combined_data$`Current Estimates`)))
    combined_data$`With your<br>changes implemented` <- ifelse(combined_data$`With your<br>changes implemented` == "", "", comma(as.numeric(combined_data$`With your<br>changes implemented`)))
    combined_data$`Current Total:` <- ifelse(combined_data$`Current Total:` == "", "", comma(as.numeric(combined_data$`Current Total:`)))
    combined_data$`Updated Total:` <- ifelse(combined_data$`Updated Total:` == "", "", comma(as.numeric(combined_data$`Updated Total:`)))
    
    # Start building the HTML table
    html_table <- '<table style="width:100%; border-collapse:collapse;" border="1">'
    
    # Define the column widths (percentage of total table width)
    col_width <- "20%"  # Set equal width for columns 2:5
    
    # Add header row with left-aligned column titles
    html_table <- paste0(html_table, '<tr>
                                     <th style="text-align:left;">£Millions</th>
                                     <th style="text-align:left; width:', col_width, ';">Current Estimates</th>
                                     <th style="text-align:left; width:', col_width, ';">Current Total:</th>
                                     <th style="text-align:left; width:', col_width, ';">With your<br>changes implemented</th>
                                     <th style="text-align:left; width:', col_width, ';">Updated Total:</th>
                                     </tr>')
    
    # Loop through each row in combined_data
    for (i in 1:nrow(combined_data)) {
      html_table <- paste0(html_table, "<tr>")
      
      # Apply shading to the first cell in the first row
      if (i == 1) {
        html_table <- paste0(html_table, '<td style="background-color:lightgray;">', combined_data[i, 1], '</td>')
      } else {
        html_table <- paste0(html_table, "<td>", combined_data[i, 1], "</td>")
      }
      
      # Add other columns for the row with right-aligned values
      html_table <- paste0(html_table, "<td style='text-align:right; width:", col_width, ";'>", combined_data[i, 2], "</td>")
      html_table <- paste0(html_table, "<td style='text-align:right; width:", col_width, ";'>", combined_data[i, 3], "</td>")
      html_table <- paste0(html_table, "<td style='text-align:right; width:", col_width, ";'>", combined_data[i, 4], "</td>")
      html_table <- paste0(html_table, "<td style='text-align:right; width:", col_width, ";'>", combined_data[i, 5], "</td>")
      
      html_table <- paste0(html_table, "</tr>")
    }
    
    # Close the table
    html_table <- paste0(html_table, "</table>")
    
    # Return the HTML table
    HTML(html_table)
  })
  
  
  output$tax_table2 <- renderDT({
    combined_data <- static_data()
    
    # Rename columns
    colnames(combined_data)[colnames(combined_data) == "Millions"] <- "£Millions"
    colnames(combined_data)[colnames(combined_data) == "current_estimates"] <- "Current Estimates"
    colnames(combined_data)[colnames(combined_data) == "updated_new"] <- "With your changes implemented"
    colnames(combined_data)[colnames(combined_data) == "est_tot"] <- "Current Total:"
    colnames(combined_data)[colnames(combined_data) == "upd_tot"] <- "Updated Total:"
    
    # Handle NAs first: Replace NAs with empty strings
    combined_data[is.na(combined_data)] <- ""
    
    # Format numeric columns with commas (skip already blank cells)
    combined_data$`Current Estimates` <- ifelse(combined_data$`Current Estimates` == "", "", comma(as.numeric(combined_data$`Current Estimates`)))
    combined_data$`With your changes implemented` <- ifelse(combined_data$`With your changes implemented` == "", "", comma(as.numeric(combined_data$`With your changes implemented`)))
    combined_data$`Current Total:` <- ifelse(combined_data$`Current Total:` == "", "", comma(as.numeric(combined_data$`Current Total:`)))
    combined_data$`Updated Total:` <- ifelse(combined_data$`Updated Total:` == "", "", comma(as.numeric(combined_data$`Updated Total:`)))
    
    datatable(combined_data, 
              options = list(
                pageLength = 10,
                columnDefs = list(
                  list(className = 'dt-left', targets = 1:4),  # Left-align headers
                  list(className = 'dt-right', targets = 2:5)  # Right-align values
                )
              )
    ) %>%
      formatStyle(columns = 1, 
                  rows = 1, 
                  backgroundColor = 'lightgray')  # Shade the first cell
  })
  
  
  output$tax_table <- renderTable({
    combined_data <- static_data()
    
    # Rename columns
    colnames(combined_data)[colnames(combined_data) == "Millions"] <- "£Millions"
    colnames(combined_data)[colnames(combined_data) == "current_estimates"] <- "Current Estimates"
    #colnames(combined_data)[colnames(combined_data) == "blank_col"] <- "               "
    colnames(combined_data)[colnames(combined_data) == "updated_new"] <- "With your changes implemented"
    colnames(combined_data)[colnames(combined_data) == "est_tot"] <- "Current Total:"
    colnames(combined_data)[colnames(combined_data) == "upd_tot"] <- "Updated Total:"
    
    # Handle NAs first: Replace NAs with empty strings
    combined_data[is.na(combined_data)] <- ""
    
    # Format numeric columns with commas (skip already blank cells)
    combined_data$`Current Estimates` <- ifelse(combined_data$`Current Estimates` == "", "", comma(as.numeric(combined_data$`Current Estimates`)))
    combined_data$`With your changes implemented` <- ifelse(combined_data$`With your changes implemented` == "", "", comma(as.numeric(combined_data$`With your changes implemented`)))
    combined_data$`Current Total:` <- ifelse(combined_data$`Current Total:` == "", "", comma(as.numeric(combined_data$`Current Total:`)))
    combined_data$`Updated Total:` <- ifelse(combined_data$`Updated Total:` == "", "", comma(as.numeric(combined_data$`Updated Total:`)))
    
    combined_data
  }, 
  rownames = FALSE,
  align = 'lrrrr'
  )
  
  
  
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
      updateNumericInput(session,"rate_1_t",value = 37500)
      updateNumericInput(session,"rate_2_t",value = 112500)
      updateNumericInput(session,"welsh_rate_1",value = 10)
      updateNumericInput(session,"welsh_rate_2",value = 10)
      updateNumericInput(session,"welsh_rate_3",value = 10)
    }
  })
  
  
  ###################################################################
  #Observe model selection to toggle the Uk rate to 10/20/30% or 0% #
  ###################################################################
  uk_rate_text_1 <- reactive({
    if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")){
      "UK: 0%"
    } else {
      "UK: 10%"
    }
  })
  uk_rate_text_2 <- reactive({
    if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")){
      "UK: 0%"
    } else {
      "UK: 30%"
    }
  })
  uk_rate_text_3 <- reactive({
    if (input$income_tax_system_choice %in% c("Scottish Model", "Full Devolution")){
      "UK: 0%"
    } else {
      "UK: 35%"
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
    if (num_rows > 0) {
      # Loop to create dynamic rows starting from the 4th row
      for (i in 1:num_rows) {
        # Create a column for the threshold part, depending on the condition
        threshold_column <- if (i == num_rows) {
          column(3,
                 
                 fluidRow(column(6,
                                 div(style = "height:18px;", p("")),
                                 tags$div(style = "font-size: 15px;font-weight: bold;", paste0("Band ", i, ":")),        
                                 
                                 
                                 ),
                          column(6,
                                 div(style = "height:10px;", p("")),
                                 #tags$div(style = "font-size: 14px;", paste0("Band ", i, ":")),
                                 div(style = "height: 1px;", p("")),
                                 tags$div(style = "font-size: 14px;", "Unlimited"))
                                 )
                          )
                 
        } else {
          column(3,
                 fluidRow(column(6,
                                 div(style = "height:18px;", p("")),
                                 tags$div(style = "font-size: 15px;font-weight: bold;", paste0("Band ", i, ":")),        
                                 
                                 
                 ),
                 column(6,
                        #tags$div(style = "font-size: 14px;", paste0("Band ", i, ":")),
                        div(style = "height:10px;", p("")),
                        div(class = "rates_input_left",
                            tags$label("£", `for` = paste0("rate_", i, "_t")),
                            numericInput(paste0("rate_", i, "_t"), NULL, 37500, step = 100))
                        )
                 )
                 
                 
             
          )
        }
        
        uk_rate_column <- if (i == 1 && input$income_tax_system_choice == "Current Settlement"){
          uk_rate = 10
        }else if (i == 2 && input$income_tax_system_choice == "Current Settlement"){
          uk_rate = 30
        }else if ((i == 3 && input$income_tax_system_choice == "Current Settlement")){
          uk_rate = 35
        }else{
          uk_rate = 0
        }
        
 
        # Add this to the rows list, along with the other columns
        rows[[i]] <- fluidRow(
 
          threshold_column,  # Use the threshold_column variable
          
          column(6,
                 fluidRow(
                   column(3,
                          div(style = "height: 18px;", p("")),
                          div(class = "grey-text-box", 
                              paste0(uk_rate,"%"))
                   ),
                   column(9,
                          div(class = "rates_input_right_slider custom-numeric-input",
                              tags$label(paste(""), `for` = paste0("welsh_rate_", i)),
                              sliderInput(paste0("welsh_rate_", i), NULL, min = 0, max = 100, value = 20, step = 1))
                   )
                 )
          ),
          column(3,
                 div(style = "height: 10px;", p("")),
                 #add the placeholder for the difference to be added later:
                 uiOutput(paste0("arrow_and_number_", i))  # Placeholder for arrow and number
          )
        )
      }
      
    }
    
    # Return the list of dynamic rows
    do.call(tagList, rows)
  })
  
  
  ################################################################################
  #Calculate the difference and add the arrow to placeholder in above function:  #
  ################################################################################
  observe( {
    #
    rate_1_t <- debounced_num_rows()
    
    num_rows <- input$num_rows
    income_tax_differences <- values$income_tax_difference_list
    
    for (i in 1:num_rows) {
      local({
        local_i <- i  # Create a local variable to capture the value of 'i' for this iteration
        
        # Calculate the difference for this row
        difference <- income_tax_differences[local_i]
        difference_rounded <- round(difference/1000000)
        formatted_difference <- format(abs(difference_rounded), big.mark = ",", scientific = FALSE)
            
        # Determine the arrow direction
        arrow_icon <- if (difference > 0) {
          tags$span(icon("arrow-up", class = "fa-2x"), style = "color: green;")
        } else if (difference == 0) {
          tags$span(icon("arrow-right", class = "fa-2x"), style = "color: blue;")
        } else {
          tags$span(icon("arrow-down", class = "fa-2x"), style = "color: red;")
        }
        
        if (difference == 0){
          difference_output <- formatted_difference
        }else{
          difference_output <- paste0(formatted_difference, " million")
        }
        
        
        
        # Update the arrow and number in the UI for this specific 'i'
        output[[paste0("arrow_and_number_", local_i)]] <- renderUI({
          tagList(
            arrow_icon,
            tags$span(style = "margin-left: 10px;", paste0("£", difference_output))
          )
        })
      })
    }
  })
  
  
  
renderArrow <- function(difference, label) {
  #print("boop:")
  #print(difference)
  
  
  
  difference_4_processing <- round(difference/1000000)
  
  # Determine the arrow direction and color
  arrow <- if (difference_4_processing > 0) {
    tags$span(icon("arrow-up", class = "fa-2x"), style = "color: green;")
  } else if (difference_4_processing == 0) {
    tags$span(icon("arrow-right", class = "fa-2x"), style = "color: blue;")
  } else {
    tags$span(icon("arrow-down", class = "fa-2x"), style = "color: red;")
  }
  
  # Format the difference output
  if (difference_4_processing == 0) {
    difference_output <- 0
  } else {
    difference_formatted <- format(round(abs(difference)/1000000), big.mark = ",", scientific = FALSE)
    difference_output <- paste0(difference_formatted, " million")
  }
  
  # Combine the arrow and the formatted number
  tagList(
    arrow,
    tags$span(style = "margin-left: 10px;", paste0("£", difference_output))
  )
}



# Define the outputs in a loop
observe({
  differences <- list(
    ndr = values$ndr_difference,
    council = values$council_difference,
    property = values$property_difference,
    tourism = values$tourism_difference,
    ltt = values$ltt_difference,
    ldt = values$ldt_difference
    # Add other differences here
  )
  
  lapply(names(differences), function(label) {
    output[[paste0(label, "_arrow")]] <- renderUI({
      renderArrow(differences[[label]], label)
    })
  })
})

  
  
  
  debounced_num_rows <- reactive({
    input$welsh_rate_1
  }) %>% debounce(5000) 
  
  

  
  
  ###############################
  #New Income Tax calculations: #
  ###############################
  calculate_income_tax_new <- reactive({
    PA <- input$pa_new
    PAlimit <- input$pa_limit
    num_rows <- input$num_rows
    
    thresholds <- numeric(num_rows -1)
    rates <- numeric(num_rows)
    
    for (i in 1:num_rows){
      rate_name <- paste0("welsh_rate_",i)
      if (input$income_tax_system_choice == "Current Settlement" && i == 1){
        rates[i] <- (input[[rate_name]] + 10)/ 100 
      }else if (input$income_tax_system_choice == "Current Settlement" && i == 2){
        rates[i] <- (input[[rate_name]] + 30)/ 100
      }else if (input$income_tax_system_choice == "Current Settlement" && i == 3){
        rates[i] <- (input[[rate_name]] + 35)/ 100
      }else{
        rates[i] <- (input[[rate_name]])/ 100
      }

      if (i == num_rows){
        break
      }else{
        threshold_name <- paste0("rate_",i,"_t")
        thresholds[i] <- input[[threshold_name]]
      }
    }
    
    
    # Import taxable income distribution
    TIDist_new <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")
    
    # Calculate Personal Allowance
    TIDist_new$PA <- ifelse(TIDist_new$TaxableIncome <= PAlimit, PA, pmax(0, PA - 0.5 * (TIDist_new$TaxableIncome - PAlimit)))
    
    # Calculate Total Taxable Income after allowance
    TIDist_new$totTaxableInc <- pmax(TIDist_new$TaxableIncome - TIDist_new$PA, 0)
    
    if (num_rows == 1){
      #this is a flat rate on all income past the PA (taxable_income)
      TIDist_new$r1_income <- pmin(TIDist_new$totTaxableInc) # First band
      TIDist_new$r1_tax <- rates[1] * TIDist_new$r1_income
      
    }else{
      
      
      # Initialize variables for tax calculations
      TIDist_new$r1_income <- pmin(TIDist_new$totTaxableInc, thresholds[1]) # First band
      TIDist_new$r1_tax <- rates[1] * TIDist_new$r1_income
      remaining_income <- TIDist_new$totTaxableInc - TIDist_new$r1_income
      
      # Loop through the remaining bands
      
      if (num_rows > 2){
        for (i in 2:length(thresholds)) {
          # Calculate the income for the current band
          band_income <- pmin(remaining_income, thresholds[i])  # Income within the current threshold
          TIDist_new[[paste0("r", i, "_income")]] <- band_income                       # Assign to dynamic column name
          TIDist_new[[paste0("r", i, "_tax")]] <- rates[i] * band_income               # Calculate tax for current band
          remaining_income <- remaining_income - band_income                       # Update remaining income
        }
      }
      # Calculate tax for income above the highest threshold
      TIDist_new[[paste0("r", length(thresholds) + 1, "_income")]] <- pmax(remaining_income, 0)
      TIDist_new[[paste0("r", length(thresholds) + 1, "_tax")]] <- rates[length(rates)] * TIDist_new[[paste0("r", length(thresholds) + 1, "_income")]]
      
    }
    
    
    # Calculate total tax payable
    TIDist_new$TotalTax <- rowSums(TIDist_new[grep("_tax$", names(TIDist_new))], na.rm = TRUE) * TIDist_new$N
    total_income_tax_new <- sum(TIDist_new$TotalTax, na.rm = TRUE)

    #Vector containing breakdown of the tax types for piechart:
    tax_columns <- grep("_tax$", names(TIDist_new), value = TRUE)  # Find column names ending with '_tax'
    tax_totals <- colSums(TIDist_new[tax_columns] * TIDist_new$N, na.rm = TRUE)    # Calculate column sums for each tax type
    
    #non_devolved_total <- 0
    if (input$income_tax_system_choice == "Current Settlement"){
      non_devolved_total <- ((0.1/rates[1]) * unname(tax_totals[1])) + ((0.3/rates[2]) * unname(tax_totals[2])) + ((0.35/rates[3]) * unname(tax_totals[3]))
      devolved_total <- total_income_tax_new - non_devolved_total
    }else{
      non_devolved_total <- 0
      devolved_total <- total_income_tax_new
    }
  
    temp_list <- vector("list", length = num_rows)
    
    for (i in 1:num_rows){
      if (i == 1){
        values$income_tax_difference_list[i] = round(unname(tax_totals[1])) - 5097834587
      }else if (i  == 2){
        values$income_tax_difference_list[i] = round(unname(tax_totals[2])) - 1648440481
      }else if (i == 3){
        values$income_tax_difference_list[i] = round(unname(tax_totals[3])) - 426016140
      }else{
        values$income_tax_difference_list[i] = round(unname(tax_totals[i]))
      }
      
    }
    
    
    
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
    
    #divide by number of people per band if button Pressed:
   

    
    #total_income_tax_new
    return(list(non_devolved_total = non_devolved_total, 
                devolved_total = devolved_total,
                tax_columns = tax_columns, 
                tax_totals=tax_totals,
                total_rates = total_rates,
                rates_list = rates_list,
                num_people = num_people))
    
  })
  
  
  #for the stacked graphs:
  #barchart:
  bar_data <- reactive({
    
    print("In the bar_data function")
    print("total_rates:")
    data <- calculate_income_tax_new()
    divisor <- if (values$divide) data$num_people else 1
    #new list to hold the divided by people:
    rates_divided_list <- vector("list", data$total_rates)
    for (i in 1:data$total_rates) {
      rates_divided_list[[i]] <- numeric(13)
    }
    
    #now to work out the new values:
    for (j in 1:data$total_rates){
      rates_divided_list[[j]] = c(data$rates_list[[j]]/divisor)
      
    }
    print(rates_divided_list)
    
    labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120+")
    
    if (values$show_sum) {
      list(
        stacked = do.call(rbind,rates_divided_list),
        labels = labels,
        legend_vector = data$tax_columns
      )
    } else {
      # Calculate sum of vectors
      summed <- Reduce(`+`,rates_divided_list)
      list(
        stacked = summed,
        labels = labels
      )
      
    }
  })
  
  # Generate Stacked Bar Chart
  output$stacked_plot_income_tax <- renderPlotly({
    print("In the stacked function:")
    data <- bar_data()
    x_categories <- factor(data$labels, levels = data$labels)
    total_rates <- input$num_rows
    
    #maybe add a warning message for when only 1 tax band is selected
    #otherwise the && here patches the bug for now.
    if (values$show_sum && total_rates > 1){
      print("In show sum:")
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
      #
      
      #
      #total_rates = 
      
      for (i in 2:total_rates){
        p <- p %>%
          add_trace(
            y = data$stacked[i,],
            name = data$legend_vector[i],
            marker = list(color = colors[i])
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
      print("Not in show sum")
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
  
  
  
  #Splitting Up income Tax function for better usability:

  #reactive data for piechart:
  reactive_income_tax_data_new <- reactive({
    
    income_tax_data <- calculate_income_tax_new()
    data.frame(
      tax_type = income_tax_data$tax_columns,
      count = income_tax_data$tax_totals
    )
  })
  
  #Income Tax piechart output:
  output$income_tax_piechart <- renderPlotly({
    income_tax_pie_data <- reactive_income_tax_data_new()
    
    
    
    income_tax_segment_colours <- c("#008080","#FF6F61", "#FFC107", "#003366", "#66B2FF", 
                                    "#E6E6FA", "#FFCC99", "#98FF98", "#CC5500", "#8E44AD")
    
    bands_list <- c("Band 1", "Band 2", "Band 3", "Band 4", "Band 5",
                    "Band 6", "Band 7", "Band 8", "Band 9", "Band 10")
    #texttemplate = ~ifelse(percentage >= 5, paste0(labels, ": ", percentage, "%"), ""),  # Show labels only if percentage >= 5%

    
    total_count <- sum(income_tax_pie_data$count)
    
    # Add percentage column calculated manually
    income_tax_pie_data$percentage <- round((income_tax_pie_data$count / total_count) * 100, 2)
    
    plot_ly(income_tax_pie_data, 
            labels = bands_list[1:length(income_tax_pie_data$count)], 
            values = ~count, 
            type = 'pie',
            textinfo = 'label+percent',
            texttemplate = ~ifelse(percentage >=1, paste0(bands_list[1:length(income_tax_pie_data$count)],": ",percentage, "%"), ""),
            marker = list(colors  = income_tax_segment_colours),
            showlegend = FALSE
            ) %>%
      layout(
        title = text_resources[[values$language]]$income_tax_pie,
        margin = list(l = 20, r = 20, b = 10, t = 30),  # Adjust margins
        paper_bgcolor = 'white',  # Background color of the plot area
        plot_bgcolor = 'white'  # Background color of the chart area
        
        #width = 200px
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #and now to download the piechart for the income tax:
  # Download handler for income tax pie chart
    output$download_income_tax_piechart <- downloadHandler(
    filename = function(){
      print("in the file function")
      paste0("income_tax_piechart_with_logo", ".png")
    },
    content = function(file){
      print("in the content fucntion")
      # Ensure pie_data is fetched correctly
      pie_data <- reactive_income_tax_data_new()
      
      # Create the pie plot using ggplot2
      pie_plot <- ggplot(pie_data, aes(x = "", y = count, fill = tax_type)) + 
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +  # Corrected coord_polar
        theme_void() + 
        theme(legend.position = "right")
      
      # Load the logo from the correct path
      logo <- readPNG("logo2.png")  # Make sure this path is correct
      
      # Convert ggplot2 plot to grob (graphical object)
      g <- ggplotGrob(pie_plot)
      
      # Open PNG device to save the image
      png(file, width = 800, height = 600, res = 100)
      
      # Draw the plot
      grid.draw(g)
      
      # Draw the logo on top (in the top-right corner)
      grid.raster(logo, x = 0.9, y = 0.9, width = unit(0.2, "npc"), height = unit(0.2, "npc"))
      
      # Close the PNG device
      dev.off()
    }
  )
  
  
  #now lets sort out the download of the figure:
  
  
  
  
  
  #call transaltion data:
  source("translations.R")
  
  # Set the language translation outputs
  set_translation_outputs(output, values, text_resources)
  

}