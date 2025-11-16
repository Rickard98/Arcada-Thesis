###
## Selection scripts
###
Relevant_topics <- c("ECON_", "MILI_", "PLO_", "CRISISLEX_", "EPU_", 'WB_1920','WB_1234','WB_696', 'WB_845','WB_1921',
                     "WB_439", "WB_471", "WB_1098","WB_444", "WB_1235", "WB_1920",'WB_318', 'WB_444', 'WB_1096', '')

Topic_pattern <- paste(Relevant_topics, collapse = "|")

#test <- Function_for_selection_publications(GDELT,Topic_pattern, Sovereing)

Function_for_selection_publications <- function(df, Sovereing){

  
  # Filter rows by the given Sovereing country name(s)
  # Filter rows by the given Sovereing country name(s)
  df <- df[grep(paste(Sovereing, collapse = "|"), df$LOCATIONS, ignore.case = TRUE), ]
  
  # Count mentions of the given Sovereing(s)
  df <- df %>%
    mutate(
      country_mentions = str_count(tolower(LOCATIONS), paste(tolower(Sovereing), collapse = "|"))
    )
  
  df <- df %>%
    distinct(CAMEOEVENTIDS, .keep_all = TRUE)
  
 df <- subset(df, country_mentions > 2)
  
 df <- select(df,LOCATIONS, THEMES, TONE, SOURCEURLS)
  
df <- df %>%
  mutate(
    num_themes = str_count(THEMES, ";") + 1  # add 1 because str_count counts separators
  )


# Count how many themes match any relevant topic per observation
df <- df %>%
  mutate(
    relevant_count = str_count(THEMES, Topic_pattern)
  )

df$relevant <- df$relevant_count/df$num_themes
df <- subset(df,relevant >= 0.15)

# Split the TONE column into multiple columns
tone_split <- t(as.data.frame(strsplit(as.character(df$TONE), ",")))

# Assign column names tone_1, tone_2, ...
colnames(tone_split) <- paste0("tone_", seq_len(ncol(tone_split)))

# Bind them back to the original dataframe
df <- cbind(df, tone_split)

# Convert the new columns to numeric
df[, grep("tone_", names(df))] <- 
  lapply(df[, grep("tone_", names(df))], as.numeric)

return(df)
}
