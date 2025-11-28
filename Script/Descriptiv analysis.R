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
  geom_line(aes(y = France, color = "france"), size = 1) +
  labs(title = "CDS Spreads",
       x = "Date",
       y = "Value",
       color = "Country") +
  theme_minimal()


ggplot(wide_df, aes(x = Date)) +
  geom_line(aes(y = US, color = "US"), size = 1) +
  geom_line(aes(y = Germany, color = "germany"), size = 1) +
  labs(title = "CDS Spreads",
       x = "Date",
       y = "Value",
       color = "Country") +
  theme_minimal()


####################################
#### GDELT DATA
####################################
library(zoo)
library(dplyr)

GDELT <- read_xlsx("Results/initial_news_sentiment_test2.xlsx")
GDELT <- read_xlsx("Results/initial_news_sentiment_test.xlsx")

GDELT <- GDELT %>% rename(country = Sovereing) %>% rename(Date = date)

 GDELT <- GDELT %>%
  mutate(Date = as.Date(as.character(GDELT$Date), format = "%Y%m%d")) %>%
  arrange(Date)

GDELT <- GDELT %>% 
  select(Date, country, matches("^tone_[1234]_"))

GDELT <- GDELT[, c("Date", "country", grep("^tone_[1]_", names(GDELT), value = TRUE))]


GDELT_france <- subset(GDELT, country == "france")

tone_cols <- names(GDELT_france)[grepl("tone", names(GDELT_france))]

GDELT_france <- GDELT_france %>%
  mutate(
    across(
      all_of(tone_cols),
      ~ rollapply(.x,
                  width = 7,
                  FUN = function(x) mean(x, na.rm = TRUE),
                  fill = NA,
                  align = "right"),
      .names = "{.col}_ma7"
    )
  )

ggplot(GDELT_france, aes(Date)) +
  geom_line(aes(y = tone_1_Economic_general, color = "Original")) +
  geom_line(aes(y = tone_1_Economic_general_ma7, color = "Smoothed (MA7)"), size = 1) +
  scale_color_manual(values = c("Original" = "grey40",
                                "Smoothed (MA7)" = "blue")) +
  labs(
    title = "Original vs Smoothed (7-day MA)",
    y = "Value", color = ""
  ) +
  theme_minimal()



### SAVE

#GDELT_france <- GDELT_france[, c("Date", "country", grep("_ma7$", names(GDELT_france), value = TRUE))]

## #CDS
France <- subset(CDS_data, country == "France")
France_GDELT_combo <- merge(France, GDELT_france, by = c("Date"))

France_GDELT_combo <- France_GDELT_combo %>%
  mutate(tone_1_Economic_general_ma7_diff =
           tone_1_Economic_general_ma7 - lag(tone_1_Economic_general_ma7))

France_GDELT_combo <- France_GDELT_combo %>%
  mutate(tone_1_Economic_policy_ma7_diff =
           tone_1_Economic_policy_ma7 - lag(tone_1_Economic_policy_ma7))

# France_GDELT_combo <- France_GDELT_combo %>%
#   mutate(tone_1_Economic_general_diff =
#            tone_1_Economic_general - lag(tone_1_Economic_general))

France_GDELT_combo <- France_GDELT_combo %>%
  mutate(CDS_Price_diff =
           Price - lag(Price))


ggplot(France_GDELT_combo, aes(Date)) +
  geom_line(aes(y = CDS_Price_diff, color = "CDS_diff")) +
  geom_line(aes(y = tone_1_Economic_general_ma7_diff, color = "Tone_diff"), size = 1) +
  scale_color_manual(values = c("CDS_diff" = "grey40",
                                "Tone_diff" = "blue")) +
  labs(
    y = "Value", color = ""
  ) +
  theme_minimal()


ggplot(France_GDELT_combo, aes(
  x = tone_1_Economic_general,
  y = Price
)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(
    x = "Tone (diff)",
    y = "CDS Price (diff)"
  ) +
  theme_minimal()

#############################
## Statistical analysis 
#############################
library(vars)
library(tseries)


var_data <- France_GDELT_combo %>%
  dplyr::select(tone_1_Economic_general_ma7_diff, CDS_Price_diff) %>%
  na.omit()

adf.test(var_data$tone_1_Economic_general_diff)
adf.test(var_data$CDS_Price_diff)


lag_selection <- VARselect(var_data, lag.max = 100, type = "const") ## ta 21
lag_selection

var_model <- VAR(var_data, p = 21, type = "const")
summary(var_model)



causality(var_model, cause = "tone_1_Economic_general_diff")



irf_result <- irf(var_model, impulse = "tone_1_Economic_general_diff",
                  response = "CDS_Price_diff",
                  n.ahead = 50, boot = TRUE)

plot(irf_result)

