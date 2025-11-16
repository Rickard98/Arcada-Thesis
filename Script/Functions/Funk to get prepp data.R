library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)

########################
## Equity
#########################

Equity_prepp <- function(Equity_data){
  Equity_long <- Equity_data %>%
    pivot_longer(
      cols = -Date,           # keep Date as is, pivot all other columns
      names_to = "Index",     # new column for index names
      values_to = "Equity_value"     # new column for values
    )
  
  
  # Define the indices and corresponding countries
  indices <- c("X.GDAXI", "FTSEMIB.MI", "X.FCHI", "X.IBEX", "X.STOXX50E")
  country <- c("Germany", "Italy", "France", "Spain", "Europe")
  Equity_ID <- data.frame(Index = indices, country = country, stringsAsFactors = FALSE)
  
  Equity_long2 <- merge(Equity_long, Equity_ID, by = "Index")
  return(Equity_long2)
  
}

########################
## Macroindicator 
#########################

Macro_indicator <- function(df) {
  
Monthly <- df
Monthly$time <- as.Date(Monthly$time)
Monthly <- subset(Monthly, 
                  (geo == "Germany" | geo == "France" | geo == "Italy" | geo == "Spain") &
                    as.Date(time) >= as.Date("2009-01-01"))

Monthly <- select(Monthly,time,`Infaltion CPI`, `Unemployment data`,geo)
Monthly <- Monthly %>% rename(Inflation =`Infaltion CPI`)%>% rename(Unemployment =`Unemployment data`)

# Create two new time columns
Monthly$Inflation_time <- as.Date(format(Monthly$time, "%Y-%m-15"))
Monthly$Unemployment_time <- as.Date(format(Monthly$time, "%Y-%m-27"))

# Start from your existing Monthly dataframe
Monthly_long <- Monthly %>%
  select(geo, Inflation_time, Unemployment_time, Inflation, Unemployment) %>%
  pivot_longer(
    cols = c(Inflation, Unemployment),
    names_to = "variable",
    values_to = "Macro_ind"
  ) %>%
  mutate(
    time = ifelse(variable == "Inflation", Inflation_time, Unemployment_time),
    time = as.Date(time),
    # optional: clean variable names
    variable = recode(variable,
                      "Inflation" = "Inflation",
                      "Unemployment" = "Unemployment")
  ) %>%
  select(geo, time, Macro_ind, variable) %>%
  arrange(geo, time, Macro_ind)

Monthly_long <- Monthly_long %>% rename(Date = time)%>% rename(country = geo)

direction_rules <- list(
  Inflation = FALSE,      
  Unemployment = FALSE
)

## Sentiment

Monthly_long <- Monthly_long %>%
  mutate(Date = as.Date(Date))

df_lagged <- Monthly_long %>%
  arrange(country, variable, Date) %>%   # explicit, safe sort
  group_by(country, variable) %>%
  mutate(
    Macro_ind_lag = lag(Macro_ind),
    change = Macro_ind - Macro_ind_lag
  ) %>%
  ungroup()

# Add change and sentiment
df_sentiment <- df_lagged %>%
  mutate(
    direction_positive = sapply(variable, function(x) direction_rules[[x]] %||% TRUE),
    sentiment = case_when(
      is.na(change) ~ 0,           # first observation
      change == 0 ~ 0,             # stable
      direction_positive & change > 0 ~ 1,
      direction_positive & change < 0 ~ -1,
      !direction_positive & change > 0 ~ -1,
      !direction_positive & change < 0 ~ 1,
      TRUE ~ 0
    )
  )

df_sentiment <- select(df_sentiment,Date, country, Macro_ind, variable, sentiment)
df_sentiment <- df_sentiment %>% rename(Macro_sentiment =sentiment)

return(df_sentiment)
}

#########################
###Central bank speech### 
######################### 

CBS_speech_func <- function(df){
CBS <- df
CBS <- CBS %>% rename(country =Country)

CBS_df <- CBS %>%
  mutate(country = ifelse(country == "DEU", "Germany",
                                  ifelse(country == "FRA", "France",
                                         ifelse(country == "ITA", "Italy",
                                                ifelse(country == "ESP","Spain",country)))))

CBS_df <- select(CBS_df,country, Date,sentiment_label ) 


# Get all countries except ECB
target_countries <- CBS_df %>%
  filter(country != "ECB") %>%
  distinct(country) %>%
  pull(country)

# Duplicate ECB rows for all target countries
ECB_expanded <- CBS_df %>%
  filter(country == "ECB") %>%
  slice(rep(1:n(), each = length(target_countries))) %>%
  mutate(country = rep(target_countries, times = nrow(.) / length(target_countries)))

# Combine back with the original data (if you still want to keep the original ECB rows)
CBS_df_extended <- bind_rows(CBS_df, ECB_expanded)

CBS_df_extended <- subset(CBS_df_extended,country != "ECB")
CBS <- CBS %>% rename(CBS_sentiment =sentiment_label)

return(CBS_df_extended)
}