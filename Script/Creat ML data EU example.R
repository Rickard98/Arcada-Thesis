library(dplyr)
source("Script/CDS prepp.R")
source("Script/Functions/Funk to get prepp data.R")

################
### Data agregattion

#####################
## CDS_data
#####################
CDS_csv_files <- list.files(path = "CDS/CDS per Soverigin", pattern = "\\.csv$", full.names = TRUE)
CDS_data <- data.frame()
CDS_data <- CDS_func(CDS_csv_files,CDS_data)

## Example
CDS_data <- subset(CDS_data, CB == "ECB")

#####################
## Intrest rates
#####################

Euribor <- read_xlsx("Daily_data_points/Euribor.xlsx")
Euribor$Time <- as.Date(Euribor$Time)

CDS_Intrest <- merge(CDS_data, Euribor, by.x = "Date", by.y = "Time")

########################
## Equity
#########################
Equity_data <- read.csv("Daily_data_points/Equity.csv")
Equity_data <- Equity_prepp(Equity_data)
Equity_data$Date <- as.Date(Equity_data$Date)

CDS_Intrest_Equity <- merge(CDS_Intrest, Equity_data, by = c("Date", "country"))

####################
### Macro indicators 
####################
Macro <- readRDS("Macroindikator_data/Monthly_data_all.R")
Macro <- Macro_indicator(Macro)
CDS_Intrest_Equity_Macro <- merge(CDS_Intrest_Equity, Macro, by = c("Date", "country"), all.x = T)

#########################
###Central bank speech### 
#########################

CBS <- read.csv("TEXT_sentiment/CBS_IT_GER_FRA_ESP.csv")
CBS_df <- CBS_speech_func(CBS)
CDS_Intrest_Equity_Macro_CBS <- merge(CDS_Intrest_Equity_Macro, CBS_df, by = c("Date", "country"), all.x = T)


#########################
###News data### 
#########################

CDS_Intrest_Equity$country <- tolower(CDS_Intrest_Equity$country)
CDS_Intrest_Equity <- select(CDS_Intrest_Equity, -Totalt, -CB)
CDS_Intrest_Equity$Date <- as.Date(CDS_Intrest_Equity$Date)

GDELT <- read_xlsx("Results/initial_news_sentiment_test.xlsx")
GDELT$date <- as.Date(as.character(GDELT$date), format = "%Y%m%d")
GDELT <- GDELT %>% rename(country = Sovereing) %>% rename(Date = date)
GDELT <- GDELT %>% 
  select(Date, country, matches("^tone_[134]_"))

CDS_GDELT <- merge(CDS_Intrest_Equity, GDELT, by = c("Date", "country"))



write_xlsx(CDS_GDELT, "ML_data/Initial_ML_data.xlsx")


