library(dplyr)
df <- readRDS("Economic data/CBS_dataset_v1.0(news data).rds") 
df$Date <- as.Date(df$Date)

df <- subset(df, Language == "English")
df <- subset(df, Date >= as.Date("2009-01-01"))

country <- df %>%
  group_by(Country) %>%
  summarise(Antalt = sum(n()))

CentralBank <- df %>%
  group_by(CentralBank) %>%
  summarise(Antalt = sum(n()))

DF_EU <- subset(df, Country == "FRA" | Country == "ITA" | Country == "ESP" | Country == "DEU" | CentralBank == "European Central Bank")
DF_EU_2 <- select(DF_EU, Title,Subtitle,Country,text,Date) 

#write.csv(DF_EU_2, "TEST.csv")
############################################### European Central Bank
 

ECB_spech <- read.csv("Economic data/all_ECB_speeches.csv", sep = "|") 
valid_dates <- !is.na(as.Date(substr(ECB_spech$date, 1, 10), format = "%Y-%m-%d"))

# Keep only valid rows
ECB_spech <- ECB_spech[valid_dates, ]

# Convert to proper Date type after filtering
ECB_spech$date <- as.Date(substr(ECB_spech$date, 1, 10), format = "%Y-%m-%d")
ECB_spech_new <- subset(ECB_spech, date >= as.Date("2024-01-01"))
ECB_spech_new$Country <- "ECB"
ECB_spech_new <- ECB_spech_new %>% rename(Title=title)%>% rename(Subtitle=subtitle)%>% rename(Date=date)%>% rename(text=contents)
ECB_spech_new_2 <- select(ECB_spech_new, Title,Subtitle,Country,text,Date) 
Final_ECB <- rbind(DF_EU_2, ECB_spech_new_2)

Final_ECB$All_info <- paste(
  ifelse(is.na(Final_ECB$Title), "", Final_ECB$Title),
  ifelse(is.na(Final_ECB$Subtitle), "", Final_ECB$Subtitle),
  ifelse(is.na(Final_ECB$text), "", Final_ECB$text),
  sep = "<>"
)


## Centiment
saveRDS(Final_ECB, "Economic data/Final_data/EU_CB_speech.rds")

test <- read.csv("TEST.csv")

table(test$sentiment_label)
