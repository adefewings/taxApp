# Define Parameters - Income Tax 2022/23
PA <- 12500 # personal allowance
PAlimit <- 100000 # limit for personal allowance

SRthreshold <- 2000
BRthreshold <- 11000 # limit for basic rate
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


num_rows <- nrow(TIDist)
counter <- 1
band_sum <- 0

results <-numeric(13)
num_people <-numeric(13)
band_people <- 0


#now we want to do a break down of the different taxes too.
basic_list <-numeric(13)
higher_list <- numeric(13)
additional_list <- numeric(13)

#scotland only
starter_list <- numeric(13)
inter_list <- numeric(13)

basic_sum <- 0
higher_sum <- 0
addional_sum <- 0

starter_sum <- 0
inter_sum <- 0


for (i in 1:num_rows){
  
  if (TIDist$TaxableIncome[i] <= (counter * 10000)){
    band_sum <- band_sum + (TIDist$TotalTax[i])
    band_people <- band_people + (TIDist$N[i])
    basic_sum <- basic_sum + (TIDist$BRtax[i] * TIDist$N[i])
    higher_sum <- higher_sum + (TIDist$HRtax[i] * TIDist$N[i])
    addional_sum <- addional_sum + (TIDist$ARtax[i] * TIDist$N[i])
    
    starter_sum <- starter_sum + (TIDist$SRtax[i] * TIDist$N[i])
    inter_sum <- inter_sum + (TIDist$IRtax[i] * TIDist$N[i])
    
  } else {
    if (counter < 13){
      results[counter] <- band_sum
      num_people[counter] <- band_people
      basic_list[counter] <- basic_sum
      higher_list[counter] <- higher_sum
      additional_list[counter] <- addional_sum
      
      starter_list[counter] <- starter_sum
      inter_list[counter] <- inter_sum
      
      counter <- counter + 1
      
      band_sum <- TIDist$TotalTax[i]
      band_people <- TIDist$N[i]
      basic_sum <- (TIDist$BRtax[i] * TIDist$N[i])
      higher_sum <- (TIDist$HRtax[i] * TIDist$N[i])
      addional_sum <- (TIDist$ARtax[i] * TIDist$N[i])
      
      starter_sum <- (TIDist$SRtax[i] * TIDist$N[i])
      inter_sum <- (TIDist$IRtax[i] * TIDist$N[i])
      
      
      
      
    }
    else{
      band_sum <- band_sum + (TIDist$TotalTax[i])
      band_people <- band_people + (TIDist$N[i])
      basic_sum <- basic_sum + (TIDist$BRtax[i] * TIDist$N[i])
      higher_sum <- higher_sum + (TIDist$HRtax[i] * TIDist$N[i])
      addional_sum <- addional_sum + (TIDist$ARtax[i] * TIDist$N[i])
      starter_sum <- starter_sum + (TIDist$SRtax[i] * TIDist$N[i])
      inter_sum <- inter_sum + (TIDist$IRtax[i] * TIDist$N[i])
      
      
      if (i == num_rows){
        results[counter] <- band_sum
        num_people[counter] <- band_people
        basic_list[counter] <- basic_sum
        higher_list[counter] <- higher_sum
        additional_list[counter] <- addional_sum
        
        starter_list[counter] <- starter_sum
        inter_list[counter] <- inter_sum
        
      }
    }
    
  }
}


labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120+")
# Bar plot
barplot(results/num_people, 
        main = "taxable contributions per salary band.",  # Title
        xlab = "Categories",           # X-axis label
        ylab = "tax contribution",               # Y-axis label
        col = "skyblue",
        names.arg = labels, 
)               # Color of the bars

print(results/num_people)
print(sum(num_people))
print(sum(TIDist$N))
print(sum(basic_list) + sum(higher_list) + sum(additional_list))
print(sum(results))

# Combine the vectors into a matrix
data_matrix <- rbind(starter_list/num_people, basic_list/num_people, inter_list/num_people, higher_list/num_people, additional_list/num_people)

# Create a stacked bar chart
barplot(
  data_matrix,
  beside = FALSE,           # Stack bars on top of each other
  col = c("lightblue", "lightgreen", "lightcoral"),  # Colors for each segment
  names.arg = labels,       # Custom labels
  legend.text = c("starter","basic", "inter","Higher", "Additional"), # Legend for the segments
  args.legend = list(x = "topright", bty = "n"),  # Position and style of the legend
  main = "Stacked Bar Chart with Multiple Vectors", # Title
  xlab = "Categories",      # X-axis label
  ylab = "Values"           # Y-axis label
)
