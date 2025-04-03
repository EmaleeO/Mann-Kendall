library(tidyverse)
library(readxl)
library(Kendall)
sharpe <- read_xlsx("2024 Historical_Sharpe_Final FFA Report.xlsx") #need to figure out how to load this with out changing the working directory. Maybe upload the sharpe tables to github
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
mk_test<- MannKendall(sharpe_2007_2025$Value) #this is not the right code. This is trying to run a mannkendall for all the values in the data set regardless of the chemical
print(mk_test)
newtable <-  MannKendall(sharpe_2007_2025$Value)%>% summarise(across(length(sharpe_2007_2025$Value), mk_test$s1, mk_test$S, mk_test$tau)) #same problem as above but trying to table the important information but still need to figure out how to run the mannkendall
print(newtable)
