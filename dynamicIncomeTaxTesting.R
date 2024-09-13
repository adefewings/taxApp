


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
