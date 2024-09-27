library(shiny)
library(shinyjs)
library(plotly) 
library(ggplot2)
library(shinyWidgets)
library(scales)
library(DT)
library(orca)
library(magick)

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