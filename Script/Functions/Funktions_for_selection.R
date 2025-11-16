

Economic_general <- c(
  "ECON-STOCKMARKET","WB_1234","WB_1921","WB_450_DEBT","ECON_INFLATION","ELECTION"
  ,"WB_471","WB_318","WB_1096", "WB_1104","ECON_TAXATION","TAX_ECON_PRICE","WB_1281_MANUFACTURING"
)

Political <- c("PLO_","WB_696", "GENERAL_GOVERNMENT","EPU_POLICY_GOVERNMENT",
               "EPU_CATS_NATIONAL_SECURITY", "EPU_POLICY_POLITICAL")

Military_cryssis <- c("CRISISLEX_","MILI_", "MILITARY")

Economic_policy <- c("EPU_POLICY","WB_439","WB_1098","WB_444","WB_1235","EPU_ECONOMY", "WB_713",
                     "WB_775", "WB_2575")

finacial <- c("WB_1920","WB_1234","WB_318", "WB_328", "WB_336", "WB_1973","WB_2530_BUSINESS_ENVIRONMENT",
              "WB_405", "WB_1045", "WB_1045")


Dummys <- c("Economic_general", "Political", "Military_cryssis", "Economic_policy", "finacial")

#df <- GDELT

###
## Selection scripts

Function_for_selection_publications <- function(df,Topic_pattern, Sovereing){
  
  first_date <- GDELT$DATE[1]
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
      num_themes = str_count(THEMES, ";")   # add 1 because str_count counts separators
    )
  
  
  # Count how many themes match any relevant topic per observation
  df <- df %>%
    mutate(
      # Split THEMES into tokens
      tokens = str_split(THEMES, ";"),
      tokens = map(tokens, ~ .x[.x != ""]),
      # Count tokens starting with any relevant prefix
      relevant_count = map_int(tokens, function(x) {
        sum(sapply(x, function(tok) any(str_starts(tok, Relevant_topics))))
      })
    ) %>%
    select(-tokens)
  
  # Create dummy column
  df$Economic_general <- ifelse(
    grepl(paste(Economic_general, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$Political <- ifelse(
    grepl(paste(Political, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$Military_cryssis <- ifelse(
    grepl(paste(Military_cryssis, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$Economic_policy <- ifelse(
    grepl(paste(Economic_policy, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$finacial <- ifelse(
    grepl(paste(finacial, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$relevant <- df$relevant_count/df$num_themes
  df <- subset(df,relevant >= 0.10)
  
  #####################################
  #####AVerage tone
  
  # if (nrow(df) > 0) {
  #   # Split the TONE column into multiple columns
  #   tone_split <- t(as.data.frame(strsplit(as.character(df$TONE), ",")))
  #   
  #   # Assign column names tone_1, tone_2, ...
  #   colnames(tone_split) <- paste0("tone_", seq_len(ncol(tone_split)))
  #   
  #   # Bind them back to the original dataframe
  #   df <- cbind(df, tone_split)
  #   
  #   # Convert the new columns to numeric
  #   df[, grep("tone_", names(df))] <- 
  #     lapply(df[, grep("tone_", names(df))], as.numeric)
  #   
  #   df$Sovereing <- Sovereing
  #   df <- dplyr::select(df, -TONE)
  #   
  #   mean_tones <- df %>%
  #     dplyr::summarise(across(starts_with("tone_"), mean, na.rm = TRUE))
  #   
  #   mean_tones$Sovereing <- Sovereing
  #   mean_tones$date <- first_date
  # } else {
  #   # Create an empty data frame with the expected columns
  #   mean_tones <- data.frame(
  #     tone_1 = NA,
  #     tone_2 = NA,
  #     tone_3 = NA,
  #     tone_4 = NA,
  #     tone_5 = NA,
  #     tone_6 = NA,
  #     Sovereing = Sovereing,
  #     date = first_date
  #   )
  # }
  
  
  if (nrow(df) > 0) {
    
    # Split the TONE column into multiple numeric columns
    tone_split <- t(as.data.frame(strsplit(as.character(df$TONE), ",")))
    colnames(tone_split) <- paste0("tone_", seq_len(ncol(tone_split)))
    df <- cbind(df, tone_split)
    df[, grep("tone_", names(df))] <- lapply(df[, grep("tone_", names(df))], as.numeric)
    
    df$Sovereing <- Sovereing
    df <- dplyr::select(df, -TONE)
    
    # ---- Compute mean tones per dummy group ----
    mean_tones_list <- list()
    
    for (dummy in Dummys) {
      dummy_col <- paste0(dummy)
      
      if (dummy_col %in% names(df)) {
        temp <- df[df[[dummy_col]] == 1, ]
        
        
        if (nrow(temp) > 0) {
          mean_tones <- temp %>%
            dplyr::summarise(across(starts_with("tone_"), mean, na.rm = TRUE))
          
          mean_tones$Sovereing <- Sovereing
          mean_tones$date <- first_date
          mean_tones$Category <- dummy
        } else {
          mean_tones <- data.frame(
            tone_1 = NA, tone_2 = NA, tone_3 = NA, tone_4 = NA,
            tone_5 = NA, tone_6 = NA,
            Sovereing = Sovereing,
            date = first_date,
            Category = dummy
          )
        }
        
        mean_tones_list[[dummy]] <- mean_tones
      }
    }
    
    # Combine all theme summaries
    mean_tones_all <- do.call(rbind, mean_tones_list)
    
  } else {
    # Empty frame fallback
    mean_tones_all <- data.frame(
      tone_1 = NA, tone_2 = NA, tone_3 = NA, tone_4 = NA,
      tone_5 = NA, tone_6 = NA,
      Sovereing = Sovereing,
      date = first_date,
      Category = NA
    )
  }
  
  mean_tones_all_wide <- mean_tones_all %>%
    pivot_wider(
      id_cols = c(date, Sovereing),   # columns that define each unique observation
      names_from = Category,          # category becomes part of column names
      values_from = starts_with("tone_"), # all tone columns are values
      names_glue = "{.value}_{Category}"  # creates names like tone_1_Economic_general
    )
  ###return the data
  
  return(mean_tones_all_wide)
}


### 



