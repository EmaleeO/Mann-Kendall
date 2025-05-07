library(tidyverse)
library(readxl)
library(Kendall)
sharpe <-  read_excel(path ="/Users/eousley/OneDrive - beringstraits.com/Desktop/2024 Historical_Sharpe_Final FFA Report.xlsx")#Fixed 
print(sharpe)
sharpe_2007_2025 <- filter(sharpe, Date >= as.Date("2007-01-01"), ) #filter for current data set
print(sharpe_2007_2025)
nondetects <- grepl("<", sharpe_2007_2025$Value)
if (any(nondetects)) {
  sharpe_2007_2025$Value[nondetects]<- as.numeric(sub("<", "", sharpe_2007_2025$Value[nondetects]))/2
} #change the non detects to 1/2 the detection limit
print(nondetects)
print(sharpe_2007_2025$Value)
sharpe_2007_2025$Value <- as.numeric(sharpe_2007_2025$Value) #make sure all values are represented as numeric
print(sharpe_2007_2025)
Grouped <- sharpe_2007_2025 %>%
  group_by(Fil, Chemical) %>%
  group_split() #grouping by well and chemical and split into separate data frames

Grouped <- sharpe_2007_2025 %>%
  group_by(Fil, Chemical) %>%
  group_split()
# grouping by well and chemical and split into separate data frames
  #I need to run the Mann-Kendall on each tible but skip the tibles with less then 4 data points



mk_test<- MannKendall(sharpe_2007_2025
                      $Value) #this is not the right code. This is trying to run a mannkendall for all the values in the data set regardless of the chemical
print(mk_test)
newtable <-  MannKendall(Grouped$Value)%>% summarise(across(length(Grouped$Value), mk_test$s1, mk_test$S, mk_test$tau)) #same problem as above but trying to table the important information but still need to figure out how to run the mannkendall
#need to make sure that Sen Slope is calculated
print(newtable)

?MannKendall
summary(Kendall(sharpe_2007_2025$Chemical, sharpe_2007_2025$Value))
newtable <-  Kendall(sharpe_2007_2025$Chemical, sharpe_2007_2025$Value %>% summarise(across(length(sharpe_2007_2025$Value)))) #chemical cant be x because x needs to be numeric

cor(sharpe_2007_2025$Chemical, sharpe_2007_2025$Value, method="kendall")
