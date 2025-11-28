library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

GDELT_raw <- read.csv("Economic data/GDELT scraping/20251028.gkg.csv", sep = ",", header = TRUE)

GDELT <- read.csv("Economic data/GDELT scraping/20251028.gkg.csv", sep = ",", header = TRUE)
GDELT_test <- select(GDELT,LOCATIONS, THEMES, TONE, SOURCEURLS)



# Split all themes by semicolon across rows
all_themes <- unlist(strsplit(GDELT$THEMES, ";"))

# Remove empty strings (caused by trailing semicolons)
all_themes <- all_themes[all_themes != ""]

# Count occurrences
theme_counts <- sort(table(all_themes), decreasing = TRUE)

# Show top themes
Dftheme_counts <- data.frame(theme_counts)

Dftheme_counts <- subset(Dftheme_counts, Freq > 10)
write_xlsx(Dftheme_counts, "TEST_all_themes.xlsx")

#########################################################
keywords <- c("Economic", "business", "inflation", "socioeconomic", "finance", "politics", "fiscal", "monetary")

# Collapse into a single pattern for grep
pattern <- paste(keywords, collapse = "|")

# Subset rows where THEMES contains any of these keywords
GDELT_fiscal <- GDELT_test[grep(pattern, GDELT_test$THEMES, ignore.case = TRUE), ]


####

germany_df <- GDELT[grep("germany", GDELT$LOCATIONS), ]
#germany_df <- germany_df[grep(pattern, germany_df$THEMES, ignore.case = TRUE), ]


# Count how many times 'germany' is mentioned per observation
germany_df <- germany_df %>%
  mutate(
    germany_mentions = str_count(tolower(LOCATIONS), "germany")
  )

germany_df <- germany_df %>%
  distinct(CAMEOEVENTIDS, .keep_all = TRUE)
germany_df <- subset(germany_df, germany_mentions > 2)

# Define relevant keywords
Relevant_topics <- c("ECON_", "MILI_", "PLO_", "CRISISLEX_", "EPU_", 
                     "WB_439", "WB_471", "WB_1098","WB_444", "WB_1235", "WB_1920")
pattern2 <- paste(Relevant_topics, collapse = "|")

#Unrelevant_topics <- c("ETHNICITY", "RELIGION","KIDNAP" )



germany_df <- germany_df %>%
  mutate(
    num_themes = str_count(THEMES, ";") + 1  # add 1 because str_count counts separators
  )


# Count how many themes per observation match any relevant topic
# Count how many themes match any relevant topic per observation
germany_df <- germany_df %>%
  mutate(
    relevant_count = str_count(THEMES, pattern2)
  )

germany_df$relevant <- germany_df$relevant_count/germany_df$num_themes
germany_df <- subset(germany_df,relevant >= 0.1)

# Split the TONE column into multiple columns
tone_split <- t(as.data.frame(strsplit(as.character(germany_df$TONE), ",")))

# Assign column names tone_1, tone_2, ...
colnames(tone_split) <- paste0("tone_", seq_len(ncol(tone_split)))

# Bind them back to the original dataframe
germany_df2 <- cbind(germany_df, tone_split)

# Convert the new columns to numeric
germany_df2[, grep("tone_", names(germany_df2))] <- 
  lapply(germany_df2[, grep("tone_", names(germany_df2))], as.numeric)

############# 
##TEST
############

# Split all themes by semicolon across rows
all_themes <- unlist(strsplit(germany_df$THEMES, ";"))

# Remove empty strings (caused by trailing semicolons)
all_themes <- all_themes[all_themes != ""]

# Count occurrences
theme_counts <- sort(table(all_themes), decreasing = TRUE)

# Show top themes
Dftheme_counts <- data.frame(theme_counts)

Dftheme_counts <- subset(Dftheme_counts, Freq > 10)
