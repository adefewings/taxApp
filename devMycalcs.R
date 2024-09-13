# Define Parameters - Income Tax 2022/23
PA <- 12500 # personal allowance
PAlimit <- 100000 # limit for personal allowance
r1_threshold <- 37500 # limit for basic rate
r2_threshold <- 112500 # limit for higher rate
r1 <- 0.2 # basic rate
r2 <- 0.4 # higher rate
r3 <- 0.45 # additional rate

# Import taxable income distribution
TIDist <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")

# Calculate Income by tax bracket
TIDist$PA <- NA
TIDist$PA[TIDist$TaxableIncome <= PAlimit] <- PA
TIDist$PA[TIDist$TaxableIncome > PAlimit] <- pmax(0, PA - 0.5 * (TIDist$TaxableIncome[TIDist$TaxableIncome > PAlimit] - PAlimit))

TIDist$totTaxableInc <- NA
TIDist$totTaxableInc[TIDist$TaxableIncome <= PA] <- 0
TIDist$totTaxableInc[TIDist$TaxableIncome > PA] <- TIDist$TaxableIncome[TIDist$TaxableIncome > PA] - TIDist$PA[TIDist$TaxableIncome > PA]

TIDist$PAincome <- pmin(TIDist$TaxableIncome, TIDist$PA)
TIDist$r1_income <- NA
TIDist$r1_income[TIDist$totTaxableInc == 0] <- 0
TIDist$r1_income[TIDist$totTaxableInc !=0] <- pmin(TIDist$totTaxableInc[TIDist$totTaxableInc != 0], r1_threshold)

TIDist$r2_income <- NA
TIDist$r2_income[TIDist$r1_income != r1_threshold] <- 0 
TIDist$r2_income[TIDist$r1_income == r1_threshold] <- pmin(TIDist$totTaxableInc[TIDist$r1_income == r1_threshold] - r1_threshold, r2_threshold)
TIDist$r3_income <- NA
TIDist$r3_income[TIDist$r2_income != r2_threshold] <- 0
TIDist$r3_income[TIDist$r2_income == r2_threshold] <- TIDist$totTaxableInc[TIDist$r2_income == r2_threshold] - r1_threshold -r2_threshold

# Calculate tax payable by tax bracket
TIDist$r1_tax <- r1 * TIDist$r1_income
TIDist$r2_tax <- r2 * TIDist$r2_income
TIDist$r3_tax <- r3 * TIDist$r3_income

# Calculate total income tax payable
TIDist$TotalTax <- (TIDist$r1_tax + TIDist$r2_tax + TIDist$r3_tax) * TIDist$N
total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
print(total_tax_sum)