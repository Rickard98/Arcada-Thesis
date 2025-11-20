

# Economic_general <- c(
#   "ECON-STOCKMARKET","WB_1921_PRIVATE_SECTOR_DEVELOPMENT","WB_450_DEBT","ECON_INFLATION",
#   ,"WB_471","WB_318","WB_1096", "WB_1104","TAX_ECON_PRICE","WB_1281_MANUFACTURING"
# )

Economic_general <- c(
  "ECON-STOCKMARKET","WB_1921_PRIVATE_SECTOR_DEVELOPMENT","ECON_INFLATION","WB_471_ECONOMIC_GROWTH", "EPU_CATS_TRADE_POLICY",
  "WB_1096_MACROECONOMIC_SUSTAINABILITY","TAX_ECON_PRICE","WB_1281_MANUFACTURING"
)

DEBT  <- c("EPU_CATS_DEBT_CEILING_GOV_SHUTDOWN", "EPU_CATS_SOVEREIGN_DEBT_CURRENCY_CRISES", "ECON_SOVEREIGN_DEBT", "WB_1104_MACROECONOMIC_VULNERABILITY_AND_DEBT")

Political <- c("WB_696_PUBLIC_SECTOR_MANAGEMENT", "GENERAL_GOVERNMENT","EPU_POLICY_GOVERNMENT",
               "EPU_CATS_NATIONAL_SECURITY", "EPU_POLICY_POLITICAL", "ELECTION")

#Military_cryssis <- c("CRISISLEX_","MILI_", "MILITARY")

Economic_policy <- c("WB_439_MACROECONOMIC_AND_STRUCTURAL_POLICIES","WB_1098_MONETARY_AND_FINANCIAL_STABILITY",
                     "WB_713_PUBLIC_FINANCE", "WB_775_TRADE_POLICY_AND_INTEGRATION", "WB_2575_TRADE_POLICY_AND_INVESTMENT_AGREEMENTS")

Monetry_policy <- c("WB_444_MONETARY_POLICY", "WB_1235_CENTRAL_BANKS", "ECON_CENTRALBANK", "EPU_POLICY_CENTRAL_BANK",
                    "EPU_POLICY_MONETARY_POLICY")

finacial <- c("WB_1920_FINANCIAL_SECTOR_DEVELOPMENT","WB_1234_BANKING_INSTITUTIONS","WB_318_FINANCIAL_ARCHITECTURE_AND_BANKING",
              "WB_336_NON_BANK_FINANCIAL_INSTITUTIONS", "WB_1045_TREASURY")

Dummys <- c("Economic_general", "Political", "Economic_policy", "finacial", "DEBT", "Monetry_policy")

#########################
## Soruces 
#########################

credible_german_sources <- c(
  "aa.com.tr", "abc.net.au", "aljazeera.com", "aol.co.uk", "aol.com", "bbc.co.uk", "bbc.com",
  "cbsnews.com", "cnn.com","dailysabah.com", "durangoherald.com", "dw.com", "english.news.cn", "euractiv.com",
  "euronews.com", "finanzen.ch", "firstpost.com", "forbes.com","foreignpolicy.com", "globenewswire.com",
  "hindustantimes.com", "hurriyetdailynews.com", "ibtimes.com", "independent.co.uk",
  "irishtimes.com", "kyivpost.com","livemint.com", "manilatimes.net", "maritime-executive.com",
  "marketscreener.com", "mymotherlode.com", "nbcnews.com", "newsweek.com", "politico.eu",
  "rte.ie", "sky.com", "swissinfo.ch", "taipeitimes.com", "theguardian.com","thestar.com.my",
  "winnipegfreepress.com", "yahoo.com", "heise.de","thelocal.de", "nordbayern.de"
)

credible_french_sources <- c(
  "9news.com.au", "aa.com.tr", "abc.net.au", "aljazeera.com",
  "bbc.co.uk", "bbc.com", "bostonglobe.com", "breakingnews.ie", "britannica.com",
  "businesstimes.com.sg", "cbc.ca", "cbsnews.com", "channelnewsasia.com", "chinadaily.com.cn",
  "cnn.com", "connexionfrance.com", "dailymaverick.co.za", "dawn.com", "deadline.com",
  "defensenews.com", "durangoherald.com", "dw.com", "euractiv.com", "euronews.com",
  "financialexpress.com", "firstpost.com", "forbes.com", "foreignpolicy.com", "fortune.com",
  "globenewswire.com", "hindustantimes.com", "hollywoodreporter.com", "hurriyetdailynews.com", "ibtimes.co.uk",
  "ibtimes.com", "independent.co.uk", "independent.ie", "indianexpress.com", "irishtimes.com",
  "jamaicaobserver.com", "japantimes.co.jp", "japantoday.com", "kyivpost.com", "latimes.com",
  "lemonde.fr", "livemint.com", "manilatimes.net", "marketscreener.com", "middleeasteye.net",
  "moroccoworldnews.com", "nbcnews.com", "npr.org", "nzherald.co.nz", "panow.com",
  "perthnow.com.au", "phys.org", "politico.eu", "rnz.co.nz", "rte.ie",
  "sbs.com.au", "scmp.com", "seattletimes.com", "sky.com", "standardmedia.co.ke",
  "straitstimes.com", "stripes.com", "swissinfo.ch", "taipeitimes.com", "tass.com",
  "theconversation.com", "thedailybeast.com", "theglobeandmail.com", "theguardian.com", "thehindu.com",
  "thejakartapost.com", "thejournal.ie", "thelocal.fr", "thenationalnews.com", "thenews.com.pk",
  "thepeninsulaqatar.com", "thestar.com.my", "thetimes.com", "theweek.in", "timeslive.co.za",
  "timesofoman.com", "upi.com", "variety.com", "vietnamnews.vn", "vogue.com",
  "watoday.com.au", "winnipegfreepress.com", "wwd.com", "yahoo.com", "ynetnews.com"
)


#df <- GDELT

####################################
## Funktion för att ta ut publiaktioner
####################################
#df <- GDELT
#Sovereing <- "germany"
#GDELT <- GDELT_test

Function_for_selection_publications_not_aggre <- function(df,Topic_pattern, Sovereing){
  
  # Filter by credible sources depending on Sovereing country
  if (tolower(Sovereing) == "germany") {
    df <- subset(df, SOURCES %in% credible_german_sources)
  } else if (tolower(Sovereing) == "france") {
    df <- subset(df, SOURCES %in% credible_french_sources)
  }
  
  
  
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
  
  # df$Military_cryssis <- ifelse(
  #   grepl(paste(Military_cryssis, collapse = "|"), df$THEMES),
  #   1, 0
  # )
  
  df$Economic_policy <- ifelse(
    grepl(paste(Economic_policy, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$finacial <- ifelse(
    grepl(paste(finacial, collapse = "|"), df$THEMES),
    1, 0
  )
  
  df$relevant <- df$relevant_count/df$num_themes
  df <- subset(df,relevant >0)
  
  
    
    # Split the TONE column into multiple numeric columns
    tone_split <- t(as.data.frame(strsplit(as.character(df$TONE), ",")))
    colnames(tone_split) <- paste0("tone_", seq_len(ncol(tone_split)))
    df <- cbind(df, tone_split)
    df[, grep("tone_", names(df))] <- lapply(df[, grep("tone_", names(df))], as.numeric)
    
    df$Sovereing <- Sovereing
    df <- dplyr::select(df, -TONE)
    
    
  ###return the data
  
  return(df)
}


### 


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
  
  # df$Military_cryssis <- ifelse(
  #   grepl(paste(Military_cryssis, collapse = "|"), df$THEMES),
  #   1, 0
  # )
  
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

