library(readxl)
library(dplyr)

# Relevant_topics <- c(
#   "ECON_", "MILI_", "PLO_", "CRISISLEX_", "EPU_","TAX_ECON_PRICE",
#   "WB_1920","WB_1234","WB_696","WB_845","WB_1921","GENERAL_GOVERNMENT",
#   "WB_439","WB_471","WB_1098","WB_444","WB_1235","EPU_POLICY_GOVERNMENT",
#   "WB_318","WB_1096","WB_450","WB_1104","ELECTION","MILITARY","WB_336","WB_713",
#   "WB_775", "WB_2575", "WB_328", "WB_336", "WB_1973","WB_2530_BUSINESS_ENVIRONMENT",
#   "WB_405", "WB_1045", "WB_1045"
# )


Economic_general <- c(
  "ECON-STOCKMARKET","WB_1921_PRIVATE_SECTOR_DEVELOPMENT","ECON_INFLATION","WB_471_ECONOMIC_GROWTH", "EPU_CATS_TRADE_POLICY",
  "WB_1096_MACROECONOMIC_SUSTAINABILITY","TAX_ECON_PRICE","WB_1281_MANUFACTURING"
)

DEBT  <- c("EPU_CATS_DEBT_CEILING_GOV_SHUTDOWN", "EPU_CATS_SOVEREIGN_DEBT_CURRENCY_CRISES", "ECON_SOVEREIGN_DEBT", "WB_1104_MACROECONOMIC_VULNERABILITY_AND_DEBT")

Political <- c("WB_696_PUBLIC_SECTOR_MANAGEMENT", "GENERAL_GOVERNMENT","EPU_POLICY_GOVERNMENT",
               "EPU_CATS_NATIONAL_SECURITY", "EPU_POLICY_POLITICAL", "ELECTION")

# Military_cryssis <- c("CRISISLEX_","MILI_", "MILITARY")


# Economic_policy <- c("EPU_POLICY","WB_439_MACROECONOMIC_AND_STRUCTURAL_POLICIES","WB_1098_MONETARY_AND_FINANCIAL_STABILITY",
#                      "EPU_ECONOMY", "WB_713_PUBLIC_FINANCE","ECON_TAXATION",
#                      "WB_775_TRADE_POLICY_AND_INTEGRATION", "WB_2575_TRADE_POLICY_AND_INVESTMENT_AGREEMENTS")
# 

Economic_policy <- c("WB_439_MACROECONOMIC_AND_STRUCTURAL_POLICIES","WB_1098_MONETARY_AND_FINANCIAL_STABILITY",
                     , "WB_713_PUBLIC_FINANCE", "WB_775_TRADE_POLICY_AND_INTEGRATION", "WB_2575_TRADE_POLICY_AND_INVESTMENT_AGREEMENTS")

Monetry_policy <- c("WB_444_MONETARY_POLICY", "WB_1235_CENTRAL_BANKS", "ECON_CENTRALBANK", "EPU_POLICY_CENTRAL_BANK",
                    "EPU_POLICY_MONETARY_POLICY")

finacial <- c("WB_1920_FINANCIAL_SECTOR_DEVELOPMENT","WB_1234_BANKING_INSTITUTIONS","WB_318_FINANCIAL_ARCHITECTURE_AND_BANKING",
              "WB_336_NON_BANK_FINANCIAL_INSTITUTIONS", "WB_1045_TREASURY")

# Business <- c("WB_1973_FINANCIAL_RISK_REDUCTION","WB_2530_BUSINESS_ENVIRONMENT",
#               "WB_405_BUSINESS_CLIMATE")


###################

Data <- read_xlsx("Results/All_publications.xlsx")


## Germany

germany <- subset(Data, Sovereing == "germany")


Journals <- germany %>%
  group_by(SOURCES) %>%
  summarise(Antal = sum(n()))

Journals <- subset(Journals, Antal >= 10)



credible_German_sources <- c(
  "aa.com.tr", "abc.net.au", "aljazeera.com", "aol.co.uk", "aol.com", "bbc.co.uk", "bbc.com",
  "cbsnews.com", "cnn.com","dailysabah.com", "durangoherald.com", "dw.com", "english.news.cn", "euractiv.com",
  "euronews.com", "finanzen.ch", "firstpost.com", "forbes.com","foreignpolicy.com", "globenewswire.com",
  "hindustantimes.com", "hurriyetdailynews.com", "ibtimes.com", "independent.co.uk",
  "irishtimes.com", "kyivpost.com","livemint.com", "manilatimes.net", "maritime-executive.com",
  "marketscreener.com", "mymotherlode.com", "nbcnews.com", "newsweek.com", "politico.eu",
  "rte.ie", "sky.com", "swissinfo.ch", "taipeitimes.com", "theguardian.com","thestar.com.my",
  "winnipegfreepress.com", "yahoo.com", "heise.de","thelocal.de", "nordbayern.de"
)

germany <- subset(germany, SOURCES %in% credible_German_sources)


# Collapse into a single pattern for grep
pattern <- paste(Monetry_policy, collapse = "|")

# Subset rows where THEMES contains any of these keywords
germany_Monetry_policy <- germany[grep(pattern, germany$THEMES, ignore.case = TRUE), ]

##############################################
## france#####################################
##############################################
france <- subset(Data, Sovereing == "france")


Journals <- france %>%
  group_by(SOURCES) %>%
  summarise(Antal = sum(n()))

Journals <- subset(Journals, Antal > 10 )

high_quality_sources_fra <- c(
  "9news.com.au", "aa.com.tr", "abc.net.au", "africanews.com", "aljazeera.com",
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

High_q_france <- subset(france, SOURCES %in% high_quality_sources_fra)

## Itlay

italy <- subset(Data, Sovereing == "italy")


Journals <- italy %>%
  group_by(SOURCES) %>%
  summarise(Antal = sum(n()))

Journals <- subset(Journals, Antal == 10 )