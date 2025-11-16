library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)


########################
## Interest rates
#########################

xlsx_files <- list.files(path = "Daily_data_points/Gather_data/", pattern = "\\.xlsx$", full.names = TRUE)
All_data <- data.frame()

for (phats in xlsx_files) {
  df <- read_xlsx(phats)
  df <- df[-c(1:6), ]
  
  df <- df %>% 
    setNames(c("Time", "1w", "1m", "3m", "6m", "12m"))
  
  df$Time <- as.Date(as.numeric(df$Time), origin = "1899-12-30")
  
  All_data <- rbind(All_data, df)
}

## Keggle_data##############################
Keggle_Data <- read.csv("Daily_data_points/euribor_daily_rates.csv")
Keggle_Data <- Keggle_Data %>% 
  setNames(c("Time", "1w", "1m", "3m", "6m", "12m"))
Keggle_Data$Time <- as.Date(Keggle_Data$Time)
Keggle_Data2 <- subset(Keggle_Data, Time < as.Date("2020-04-01"))

############################################
Final_df <- rbind(Keggle_Data2, All_data)
write_xlsx(Final_df,"Daily_data_points/Euribor.xlsx")

