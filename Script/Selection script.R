library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(purrr)

source("Script/Functions/Funktions_for_selection.R")

###
## Select setting 
##

Relevant_topics <- c(
  "ECON_", "MILI_", "PLO_", "CRISISLEX_", "EPU_","TAX_ECON_PRICE",
  "WB_1920","WB_1234","WB_696","WB_845","WB_1921","GENERAL_GOVERNMENT",
  "WB_439","WB_471","WB_1098","WB_444","WB_1235","EPU_POLICY_GOVERNMENT",
  "WB_318","WB_1096","WB_450","WB_1104","ELECTION","MILITARY","WB_336","WB_713",
  "WB_775", "WB_2575", "WB_328", "WB_336", "WB_1973","WB_2530_BUSINESS_ENVIRONMENT",
  "WB_405", "WB_1045", "WB_1045"
)

Relevant_topics <- Relevant_topics[Relevant_topics != ""]

Sovereings  <- c("germany","france", "italy","spain")


### Get the data 
GDELT <- read.csv("Economic data/GDELT scraping/20251028.gkg.csv", sep = ",", header = TRUE)
files <- list.files("Economic data/GDELT scraping/", full.names = TRUE)
All_data <- data.frame()

for (path in files) {
  
GDELT <- read.csv(path)

for (Sovereing in Sovereings) {
  data <- Function_for_selection_publications(GDELT,Topic_pattern, Sovereing)
  All_data <- rbind(All_data, data)
}

}

write_xlsx(All_data, "Results/initial_news_sentiment_test.xlsx")
