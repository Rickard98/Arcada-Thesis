library(dplyr)
library(ggplot2)
library(writexl)

source("Script/CDS prepp.R")
source("Script/Functions/Funk to get prepp data.R")


#####################
## CDS_data
#####################
CDS_csv_files <- list.files(path = "CDS/CDS per Soverigin", pattern = "\\.csv$", full.names = TRUE)
CDS_data <- data.frame()
CDS_data <- CDS_func(CDS_csv_files,CDS_data)

## Example
#CDS_data <- subset(CDS_data, CB == "ECB")
CDS_data$Price <- as.numeric(CDS_data$Price)

wide_df <- CDS_data %>%
  select(Date, country, Price) %>%
  pivot_wider(names_from = country, values_from = Price)


wide_df <- subset(wide_df, as.Date(Date) >= as.Date("2013-01-01") & as.Date(Date) <= as.Date("2025-10-01"))



ggplot(wide_df, aes(x = Date)) +
  geom_line(aes(y = Germany, color = "Germany"), size = 1) +
  labs(title = "CDS Spreads",
       x = "Date",
       y = "Value",
       color = "Country") +
  theme_minimal()


ggplot(wide_df, aes(x = Date)) +
  geom_line(aes(y = US, color = "US"), size = 1) +
  geom_line(aes(y = UK, color = "UK"), size = 1) +
  labs(title = "CDS Spreads",
       x = "Date",
       y = "Value",
       color = "Country") +
  theme_minimal()

# Australian       Brazil       Canada        China        Egypt       France 
# 4335         4374         1914         4349         4322         4365 
# Germany        India    Indonesia       Israel        Italy        Japan 
# 4324         1722         4358         4103         4369         3046 
# Mexico Saudi arabia  South Afrca  South Korea        Spain  Switzerland 
# 4371         3802         4359         4372         4372          558 
# Turkey           UK           US 
# 4366         4368         3824 
