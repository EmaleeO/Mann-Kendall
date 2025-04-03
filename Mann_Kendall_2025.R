library(tidyverse)
library(readxl)
library(Kendall)
sharpe <- read_xlsx("2024 Historical_Sharpe_Final FFA Report.xlsx")
print(sharpe)
sharpe_2007_2025 <- filter(sharpe, Date >= as.Date("2007-01-01"), )
print(sharpe_2007_2025)
nondetects <- grepl("<", sharpe_2007_2025$Value)
if (any(nondetects)) {
  sharpe_2007_2025$Value[nondetects]<- as.numeric(sub("<", "", sharpe_2007_2025$Value[nondetects]))/2
}
print(nondetects)
print(sharpe_2007_2025$Value)
sharpe_2007_2025$Value <- as.numeric(sharpe_2007_2025$Value)
mk_test<- MannKendall(sharpe_2007_2025$Value)
print(mk_test)
newtable <-  MannKendall(sharpe_2007_2025$Value)%>% summarise(across(length(sharpe_2007_2025$Value), mk_test$s1, mk_test$S, mk_test$tau))
print(newtable)
