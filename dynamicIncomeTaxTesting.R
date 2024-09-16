


# Define Parameters - Income Tax 2022/23 with dynamic bands
PA <- 12500          # Personal allowance
PAlimit <- 100000    # Limit for personal allowance

# Define initial thresholds and rates
thresholds <- c(37500, 112500)   # Define thresholds for all bands
rates <- c(0.2, 0.4, 0.45)       # Define rates for each band

# Import taxable income distribution
TIDist <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")

# Calculate Personal Allowance
TIDist$PA <- ifelse(TIDist$TaxableIncome <= PAlimit, PA, pmax(0, PA - 0.5 * (TIDist$TaxableIncome - PAlimit)))

# Calculate Total Taxable Income after allowance
TIDist$totTaxableInc <- pmax(TIDist$TaxableIncome - TIDist$PA, 0)

# Initialize variables for tax calculations
TIDist$r1_income <- pmin(TIDist$totTaxableInc, thresholds[1]) # First band
TIDist$r1_tax <- rates[1] * TIDist$r1_income
remaining_income <- TIDist$totTaxableInc - TIDist$r1_income

# Loop through the remaining bands
for (i in 2:length(thresholds)) {
  # Calculate the income for the current band
  band_income <- pmin(remaining_income, thresholds[i])  # Income within the current threshold
  TIDist[[paste0("r", i, "_income")]] <- band_income                       # Assign to dynamic column name
  TIDist[[paste0("r", i, "_tax")]] <- rates[i] * band_income               # Calculate tax for current band
  remaining_income <- remaining_income - band_income                       # Update remaining income
}

# Calculate tax for income above the highest threshold
TIDist[[paste0("r", length(thresholds) + 1, "_income")]] <- pmax(remaining_income, 0)
TIDist[[paste0("r", length(thresholds) + 1, "_tax")]] <- rates[length(rates)] * TIDist[[paste0("r", length(thresholds) + 1, "_income")]]

# Calculate total tax payable
TIDist$TotalTax <- rowSums(TIDist[grep("_tax$", names(TIDist))], na.rm = TRUE) * TIDist$N
total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)

print(total_tax_sum)
tax_columns <- grep("_tax$", names(TIDist), value = TRUE)  # Find column names ending with '_tax'
tax_totals <- colSums(TIDist[tax_columns] * TIDist$N, na.rm = TRUE)    # Calculate column sums for each tax type
print(tax_columns)

#########################################
#now to do the bar chart calcualtions:  #
#########################################
num_rows <- nrow(TIDist)
counter <- 1
band_sum <- 0
results <-numeric(13)
num_people <-numeric(13)
band_people <- 0

total_rates <- 3
rates_list <- vector("list", total_rates)  # Create a list with num_rows slots
sum_list <- vector("list",total_rates)


# Populate each sublist with numeric(13)
for (i in 1:total_rates) {
  rates_list[[i]] <- numeric(13)
  sum_list[[i]] <- 0
}

#loop through tax bands to populate the variables:
for (i in 1:num_rows){
  #all bands except the last one:
  if (TIDist$TaxableIncome[i] <= (counter * 10000)){
    band_sum <- band_sum + (TIDist$TotalTax[i])
    band_people <- band_people + (TIDist$N[i])
    
    for (j in 1:total_rates){
      tax_type <- paste0("r",j,"_tax")
      sum_list[[j]] <- sum_list[[j]] + (TIDist[[tax_type]][i] * TIDist$N[i])
      #print(tax_type)
      #print(sum_list[[j]])
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
      
      band_sum <- TIDist$TotalTax[i]
      band_people <- TIDist$N[i]
      
      for (j in 1:total_rates){
        tax_type <- paste0("r",j,"_tax")
        sum_list[[j]] <- (TIDist[[tax_type]][i] * TIDist$N[i])
      }
      
    }
    #last band save:
    else{
      band_sum <- band_sum + (TIDist$TotalTax[i])
      band_people <- band_people + (TIDist$N[i])
      
      for (j in 1:total_rates){
        tax_type <- paste0("r",j,"_tax")
        sum_list[[j]] <- sum_list[[j]] + (TIDist[[tax_type]][i] * TIDist$N[i])
      }

      
      if (i == num_rows){
        results[counter] <- band_sum
        num_people[counter] <- band_people
        
        for (j in 1:total_rates){
          rates_list[[j]][counter] <- sum_list[[j]]
        }

      }
    }
  }
}




print(rates_list)

for (j in 1:total_rates){
  print(paste0("rate: ", j))
  for (p in 1:13){
    
    print(rates_list[[j]][p])
  }
  
}


#divide by number of people per band if button Pressed:
divisor <- 1
#barchart:

  
  
  #new list to hold the divided by people:
  rates_divided_list <- vector("list", total_rates)
  for (i in 1:total_rates) {
    rates_divided_list[[i]] <- numeric(13)
  }
  
  #now to work out the new values:
  for (j in 1:total_rates){
    rates_divided_list[[j]] = c(rates_list[[j]]/divisor)
    print(rates_divided_list[[j]])
    
  }
  summed <- Reduce(`+`,rates_divided_list)
  print(summed)
  #vector1 <- c(starter_list/divisor)
  #vector2 <- c(basic_list/divisor)
  #vector3 <- c(inter_list/divisor)
  #vector4 <- c(higher_list/divisor)
  #vector5 <- c(additional_list/divisor)
  
  labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120+")
  #legend_vector = c(text_resources[[values$language]]$starter,text_resources[[values$language]]$basic, text_resources[[values$language]]$intermediate,text_resources[[values$language]]$higher, text_resources[[values$language]]$additional)
  
  #if (values$show_sum) {
   # list(
      #stacked = rbind(vector1, vector2, vector3,vector4, vector5),
      
    #  stacked = do.call(rbind,rates_divided_list),
     # labels = labels,
      #legend_vector = legend_vector
    #)
  #} else {
    # Calculate sum of vectors
    #summed <- vector1 + vector2 + vector3 + vector4 + vector5
   # summed <- Reduce(`+`,rates_divided_list)
  #  list(
  #    #stacked = summed,
   #   stacked = summed,
  #    labels = labels
  #  )
    
  #}


