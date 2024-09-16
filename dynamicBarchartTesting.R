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
print(tax_totals)

#=============================================

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
    basic_sum <- basic_sum + (TIDist$r1_tax[i] * TIDist$N[i])
    higher_sum <- higher_sum + (TIDist$r2_tax[i] * TIDist$N[i])
    addional_sum <- addional_sum + (TIDist$r3_tax[i] * TIDist$N[i])
    
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
      basic_sum <- (TIDist$r1_tax[i] * TIDist$N[i])
      higher_sum <- (TIDist$r2_tax[i] * TIDist$N[i])
      addional_sum <- (TIDist$r3_tax[i] * TIDist$N[i])
      
      
    }
    else{
      #Last band tax sums:
      band_sum <- band_sum + (TIDist$TotalTax[i])
      band_people <- band_people + (TIDist$N[i])
      basic_sum <- basic_sum + (TIDist$r1_tax[i] * TIDist$N[i])
      higher_sum <- higher_sum + (TIDist$r2_tax[i] * TIDist$N[i])
      addional_sum <- addional_sum + (TIDist$r3_tax[i] * TIDist$N[i])
      
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

print(basic_list)
print(higher_list)
print(additional_list)

#for (i in 1:13){
#  print(basic_list[i])
#  print(higher_list[i])
#  print(additional_list[i])
#}

