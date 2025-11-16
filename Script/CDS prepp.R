library(readxl)
library(dplyr)


## CDS Spreads 

CDS_func <- function(CDS_csv_files, df) {

CDS_INDEXES <- read_xlsx("CDS/CDS_index.xlsx")  
All_data <- df

for (paths in CDS_csv_files) {
  cds <- read.csv(paths)
  cds <- cds %>% rename(Change = Change..)
  cds <- select(cds,Date,Price, Change )
  ###
  cds_index <- sub("=R.*", "", basename(paths))
  cds_index <- sub("\\.csv$", "", cds_index)
  
  cds$CDS_index <- cds_index
  cds <- merge(cds, CDS_INDEXES, by = "CDS_index", all.x = TRUE)
  cds$Date <- as.Date(cds$Date, format = "%m/%d/%Y")
  
  
  ##
  
  All_data <- rbind(All_data, cds)  
  
 ### Names and regions 
  
 ###
}
return(All_data)

}


