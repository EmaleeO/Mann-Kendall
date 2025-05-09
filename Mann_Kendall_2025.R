
library(tidyverse)
library(readxl)
library(Kendall)
library(openxlsx)
library(zyp)

sharpe <- read_excel("/Users/eousley/OneDrive - beringstraits.com/Desktop/2024 Historical_Sharpe_Final FFA Report.xlsx")

sharpe_2007_2025 <- sharpe %>%
  filter(Date >= as.Date("2007-01-01"))

# Replace nondetects
nondetects <- grepl("<", sharpe_2007_2025$Value)
if (any(nondetects)) {
  values_cleaned <- suppressWarnings(as.numeric(sub("<", "", sharpe_2007_2025$Value[nondetects])))
  sharpe_2007_2025$Value[nondetects] <- ifelse(is.na(values_cleaned), NA, values_cleaned / 2)
}
sharpe_2007_2025$Value <- as.numeric(sharpe_2007_2025$Value)
sharpe_2007_2025$Date <- as.Date(sharpe_2007_2025$Date)

Grouped <- sharpe_2007_2025 %>%
  group_by(Fil, Chemical) %>%
  group_split()

MK_Results <- purrr::map(Grouped, ~ {
  df <- .x %>%
    filter(!is.na(Value), !is.na(Date)) %>%
    arrange(Date)
  
  if (nrow(df) < 4) return(NULL)
  
  tryCatch({
    mk <- MannKendall(df$Value)
    
    # Convert date to numeric days since first date
    df$DateNum <- as.numeric(df$Date - min(df$Date))
    
    sen_slope <- tryCatch({
      sen <- zyp.sen(Value ~ DateNum, data = df)
      sen$coefficients[2]
    }, error = function(e) {
      NA_real_
    })
    
    tibble(
      Fil = unique(df$Fil),
      Chemical = unique(df$Chemical),
      S = mk$S,
      Tau = mk$tau,
      P_Value = mk$sl,
      Sen_Slope = sen_slope
    )
  }, error = function(e) {
    message("Mann-Kendall error in group: ", unique(df$Fil), " - ", unique(df$Chemical),
            "\nMessage: ", conditionMessage(e))
    NULL
  })
}) %>% purrr::compact()

mk_combined <- bind_rows(MK_Results)
mk_cleaned <- mk_combined %>%
  filter(S != 0)

print(mk_cleaned)

write.xlsx(mk_cleaned, file = "MK_Combined_Results.xlsx",
           sheetName = "MK Results", overwrite = TRUE)
