set_translation_outputs <- function (output, values, text_resources){
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
  output$other_taxes_intro <- renderText({
    text_resources[[values$language]]$other_taxes_intro
  })
  
}