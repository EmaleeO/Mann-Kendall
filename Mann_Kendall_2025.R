library(tidyverse)
library(readxl)
library(Kendall)
library(openxlsx)
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
  group_split() %>% #grouping by well and chemical and split into separate data frames


MK_Results <- purrr::map(Grouped, ~ {
  df <- .x  # Current group
  
  # Skip groups with fewer than 4 rows
  if (nrow(df) < 4) return(NULL)
  
  # Try-catch to handle any errors during MannKendall and return results
  tryCatch({
    mk <- MannKendall(df$Value)  # Perform Mann-Kendall test
    tibble(
      Fil = unique(df$Fil),
      Chemical = unique(df$Chemical),
      S = mk$S,
      Tau = mk$tau,
      P_Value = mk$sl
    )
  }, error = function(e) {
    message("Error in group: ", unique(df$Fil), " - ", unique(df$Chemical), 
            "\nMessage: ", conditionMessage(e))
    NULL  # Return NULL on error
  })
}) %>% purrr::compact()  # Remove NULL results after the map

# Print MK_Results to check
print(MK_Results) #IT WORKS YAY!!! Need to combine them into one tible!!! And add sen slope!!!!!!!!!!!!
mk_combined <- bind_rows(MK_Results) #binds the result rows together

write.xlsx(mk_combined, file = "MK_Combined_Results.xlsx", sheetName = "MK Results", overwrite = TRUE) #creates and Excel file that saves to your working directory



