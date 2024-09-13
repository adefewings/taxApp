# Define Parameters - Income Tax 2022/23
PA <- 12500 # personal allowance
PAlimit <- 100000 # limit for personal allowance
BRthreshold <- 37500 # limit for basic rate
HRthreshold <- 150000 # limit for higher rate
BR <- 0.2 # basic rate
HR <- 0.4 # higher rate
AR <- 0.45 # additional rate

# Import taxable income distribution
TIDist <- read.csv("TaxableIncomeDistribution2023.csv", sep=";")

# Calculate Income by tax bracket
TIDist$PA <- NA
TIDist$PA[TIDist$TaxableIncome <= PAlimit] <- PA
TIDist$PA[TIDist$TaxableIncome > PAlimit] <- pmax(0, PA - 0.5 * (TIDist$TaxableIncome[TIDist$TaxableIncome > PAlimit] - PAlimit))
TIDist$PAincome <- pmin(TIDist$TaxableIncome, TIDist$PA)
TIDist$BRincome <- NA
TIDist$BRincome[TIDist$PAincome < TIDist$PA] <- 0
TIDist$BRincome[TIDist$PAincome >= TIDist$PA] <- pmin(TIDist$TaxableIncome[TIDist$PAincome >= TIDist$PA] - TIDist$PA[TIDist$PAincome >= TIDist$PA], BRthreshold)
TIDist$HRincome <- NA
TIDist$HRincome[TIDist$BRincome < BRthreshold] <- 0
TIDist$HRincome[TIDist$BRincome == BRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$BRincome == BRthreshold] - TIDist$PA[TIDist$BRincome == BRthreshold] - BRthreshold, HRthreshold - BRthreshold)
TIDist$ARincome <- NA
TIDist$ARincome[TIDist$HRincome < HRthreshold - BRthreshold] <- 0
TIDist$ARincome[TIDist$HRincome == HRthreshold - BRthreshold] <- TIDist$TaxableIncome[TIDist$HRincome == HRthreshold - BRthreshold] - TIDist$PA[TIDist$HRincome == HRthreshold - BRthreshold] - HRthreshold

# Calculate tax payable by tax bracket
TIDist$BRtax <- BR * TIDist$BRincome
TIDist$HRtax <- HR * TIDist$HRincome
TIDist$ARtax <- AR * TIDist$ARincome

# Calculate total income tax payable
TIDist$TotalTax <- (TIDist$BRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
print(total_tax_sum)

