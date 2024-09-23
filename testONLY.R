app_parameters_data <- read.csv("appdata.csv", header = FALSE, stringsAsFactors = FALSE)
app_parameters_list <- setNames(as.list(app_parameters_data$V2), app_parameters_data$V1)
print(app_parameters_list$current_other )