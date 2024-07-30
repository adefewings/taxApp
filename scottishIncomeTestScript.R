# Define Parameters - Income Tax 2022/23
PA <- 12500 # personal allowance
PAlimit <- 100000 # limit for personal allowance

SRthreshold <- 2000
BRthreshold <- 11500 # limit for basic rate
IRthreshold <- 18000
HRthreshold <- 106000 # limit for higher rate

ARstartAmount <- 150000

SR <- 0.19 # starter rate
BR <- 0.2 # basic rate
IR <- 0.21 #intermediate rate
HR <- 0.41 # higher rate
AR <- 0.46 # additional rate

TIDist$PA <- NA
TIDist$PA[TIDist$TaxableIncome <= PAlimit] <- PA
TIDist$PA[TIDist$TaxableIncome > PAlimit] <- pmax(0, PA - 0.5 * (TIDist$TaxableIncome[TIDist$TaxableIncome > PAlimit] - PAlimit))
TIDist$PAincome <- pmin(TIDist$TaxableIncome, TIDist$PA)

TIDist$SRincome <- NA
TIDist$SRincome[TIDist$PAincome < TIDist$PA] <- 0 
TIDist$SRincome[TIDist$PAincome >= TIDist$PA] <- pmin(TIDist$TaxableIncome[TIDist$PAincome >= TIDist$PA] - TIDist$PA[TIDist$PAincome >= TIDist$PA], SRthreshold)

TIDist$BRincome <- NA
TIDist$BRincome[TIDist$SRincome < SRthreshold] <- 0
TIDist$BRincome[TIDist$SRincome >= SRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$SRincome >= SRthreshold] - TIDist$PA[TIDist$SRincome >= SRthreshold] - SRthreshold, BRthreshold)

TIDist$IRincome <- NA
TIDist$IRincome[TIDist$BRincome < BRthreshold] <- 0
TIDist$IRincome[TIDist$BRincome >= BRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$BRincome >= BRthreshold] - TIDist$PA[TIDist$BRincome >= BRthreshold] - SRthreshold - BRthreshold, IRthreshold)

TIDist$HRincome <- NA
TIDist$HRincome[TIDist$IRincome < IRthreshold] <- 0
TIDist$HRincome[TIDist$IRincome >= IRthreshold] <- pmin(TIDist$TaxableIncome[TIDist$IRincome >= IRthreshold] - TIDist$PA[TIDist$IRincome >= IRthreshold] - SRthreshold - BRthreshold - IRthreshold, HRthreshold)

TIDist$ARincome <- NA
TIDist$ARincome[TIDist$HRincome < HRthreshold] <- 0
TIDist$ARincome[TIDist$HRincome >= HRthreshold] <- TIDist$TaxableIncome[TIDist$HRincome >= HRthreshold] - TIDist$PA[TIDist$HRincome >= HRthreshold] - SRthreshold - BRthreshold - IRthreshold - HRthreshold

# Calculate tax payable by tax bracket
TIDist$SRtax <- SR * TIDist$SRincome
TIDist$BRtax <- BR * TIDist$BRincome
TIDist$IRtax <- IR * TIDist$IRincome
TIDist$HRtax <- HR * TIDist$HRincome
TIDist$ARtax <- AR * TIDist$ARincome



# Calculate total income tax payable

TIDist$TotalTax <- (TIDist$SRtax + TIDist$BRtax + TIDist$IRtax + TIDist$HRtax + TIDist$ARtax) * TIDist$N
total_tax_sum <- sum(TIDist$TotalTax, na.rm = TRUE)
print(total_tax_sum)

