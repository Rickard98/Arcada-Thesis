library(dplyr)

GDELT_smal_org <- read.csv("Economic data/GDELT/20251025.gkg.csv", sep = "\t", header = TRUE)
GDELT_big <- read.csv("Economic data/GDELT/20251025.gkgcounts.csv", sep = "\t", header = TRUE)
GDELT <- read.csv("Economic data/GDELT/20251103.export.CSV", sep = "\t", header = TRUE)
GDELT <- read.csv("Economic data/GDELT/20251122084500.export.CSV", sep = "\t", header = TRUE)

GDELT <- read.csv("Economic data/GDELT/20251121100000.gkg.csv", sep = "\t", header = TRUE)
colnames(GDELT) <- paste0("column", seq_along(GDELT))


GDELT_econ <- GDELT[grep("9.297", GDELT$column16), ]



GDELT_smal <- read.csv("Economic data/GDELT scraping/20251025.gkg.csv", sep = ",", header = TRUE)


sum(sapply(GDELT_smal$LOCATIONS, function(x) {
  length(grep("Germany", unlist(strsplit(x, ";"))))
}))

GDELT_smal$LOCATIONS <- tolower(GDELT_smal$LOCATIONS)
germany_df <- GDELT_smal[grep("germany", GDELT_smal$LOCATIONS), ]
germany_df <- GDELT_smal[grep("germany|france|italy|spain", GDELT_smal$LOCATIONS, ignore.case = TRUE), ]

germany_df <- germany_df[!is.na(germany_df$THEMES) & germany_df$THEMES != "", ]


Topics <- germany_df %>%
  group_by(THEMES)%>%
  summarise(Totalt = n())

# Define keywords related to economy and politics
keywords <- c("Economic", "business", "inflation", "socioeconomic", "finance", "politics", "government", "policy", "fiscal", "monetary")

# Collapse into a single pattern for grep
pattern <- paste(keywords, collapse = "|")

# Subset rows where THEMES contains any of these keywords
germany_economic_df <- germany_df[grep(pattern, germany_df$THEMES, ignore.case = TRUE), ]

Topics <- germany_economic_df %>%
  group_by(THEMES)%>%
  summarise(Totalt = n())
