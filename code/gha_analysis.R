# Setup ####
list.of.packages <- c("data.table", "rstudioapi", "stringr", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("code/download.R")
file_location = download_foreignassistance()
usaid = fread(file_location)
usaid$year = as.numeric(substr(as.character(usaid$`Transaction Date`), 6, 9))
usaid = subset(usaid, year %in% c(2022, 2023))
usaid = subset(usaid, `Transaction Type Name`=="Disbursements")

dac_humanitarian = subset(usaid, `International Sector Code` %in% c(720, 730, 740))
us_humanitarian = subset(usaid, `US Category Name` == "Humanitarian Assistance")

dac_hum_agg = dac_humanitarian[,.(
  # dac_humanitarian_usd_disbursement=sum(`Current Dollar Amount`),
  dac_humanitarian_usd_disbursement_deflated=sum(`Constant Dollar Amount`)
),by=.(year, `Country Code`, `Country Name`)]

us_hum_agg = us_humanitarian[,.(
  # us_humanitarian_usd_disbursement=sum(`Current Dollar Amount`),
  us_humanitarian_usd_disbursement_deflated=sum(`Constant Dollar Amount`)
),by=.(year, `Country Code`, `Country Name`)]

hum_agg = merge(dac_hum_agg, us_hum_agg, by=c("year", "Country Code", "Country Name"), all=T)
fwrite(hum_agg, "usaid_humanitarian_comparison_22_23.csv")
