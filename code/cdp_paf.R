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

# Keyword search ####

quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

remove_punct = function(string){
  str_replace_all(string, "[[:punct:]]", " ")
}

collapse_whitespace = function(string){
  str_replace_all(string, "\\s+", " ")
}

countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }

usaid = subset(usaid, `Fiscal Year` >= 2017)

usaid = usaid[,c("Activity Name", "Activity Description", "Activity Project Number")]

usaid = unique(usaid)

usaid$text = paste(usaid$`Activity Name`, usaid$`Activity Description`)
usaid$text = trimws(collapse_whitespace(remove_punct(tolower(usaid$text))))
usaid$text_len = countSpaces(usaid$text)
median(usaid$text_len)
gc()

keywords = fread("~/git/cdp-paf/data/keywords.csv")
keywords$keyword = quotemeta(trimws(collapse_whitespace(remove_punct(tolower(keywords$keyword)))))
# Exclude relief for now because extra logic for bad relief and debt relief
keywords = subset(keywords, keyword != "relief")

paf_keywords = subset(keywords, category!="CF")$keyword
paf_regex = paste0(
  "\\b",
  paste(paf_keywords, collapse="\\b|\\b"),
  "\\b"
)
aa_keywords = subset(keywords, !category %in% c("CF", "PAF"))$keyword
aa_regex = paste0(
  "\\b",
  paste(aa_keywords, collapse="\\b|\\b"),
  "\\b"
)

usaid$`PAF keyword match` = grepl(paf_regex, usaid$text, perl=T, ignore.case = T)
usaid_paf = subset(usaid, `PAF keyword match`)

usaid_paf$`AA keyword match` = grepl(aa_regex, usaid_paf$text, perl=T, ignore.case = T)
usaid_aa = subset(usaid_paf, `AA keyword match`)

paf = read.xlsx(
  "~/git/cdp-paf/large_data/Full PAF dataset enhanced 2024_slim.xlsx",
  sheet="PAF"
)
paf = subset(paf, donor_name == "United States")
paf_titles = unique(paf$project_title)

aa = read.xlsx(
  "~/git/cdp-paf/large_data/Full PAF dataset enhanced 2024_slim.xlsx",
  sheet="AA"
)
aa = subset(aa, donor_name == "United States")
aa_titles = unique(aa$project_title)
gc()

usaid_paf = subset(usaid_paf, !`Activity Name` %in% paf_titles)
usaid_aa = subset(usaid_aa, !`Activity Name` %in% aa_titles)

potential_false_positive_titles = c()
for(year in 2017:2022){
  tmp = fread(paste0("~/git/cdp-paf/large_data/crs_",year,"_cdp_automated.csv"))
  tmp = subset(tmp, donor_name=='United States' & `PAF keyword match` == TRUE)
  potential_false_positive_titles = c(potential_false_positive_titles, unique(tmp$project_title))
  rm(tmp)
  gc()
}
potential_false_positive_titles = unique(potential_false_positive_titles)

usaid_paf = subset(usaid_paf, !`Activity Name` %in% potential_false_positive_titles)
usaid_aa = subset(usaid_aa, !`Activity Name` %in% potential_false_positive_titles)


crs = fread("large_input/usaid_crs.csv")
crs_titles = unique(crs$project_title)

usaid_paf$found_in_crs = (usaid_paf$`Activity Name` %in% crs_titles)
usaid_aa$found_in_crs = (usaid_aa$`Activity Name` %in% crs_titles)

usaid_paf_short = unique(usaid_paf[,c("found_in_crs", "Activity Name", "Activity Description","text","Activity Project Number")])
usaid_aa_short = unique(usaid_aa[,c("found_in_crs", "Activity Name", "Activity Description","text","Activity Project Number")])

bool_to_str = function(x){
  x[which(x==F)] = NA
  return(x)
}

for(paf_keyword in paf_keywords){
  message(paf_keyword)
  keyword_regex = paste0("\\b", paf_keyword, "\\b")
  usaid_paf_short[,paf_keyword] = grepl(keyword_regex, usaid_paf_short$text, perl=T, ignore.case = T)
  usaid_paf_short[,paf_keyword] = bool_to_str(usaid_paf_short[,paf_keyword,with=F])
}
usaid_paf_short$text = NULL

crs$text = paste(crs$project_title, crs$short_description, crs$long_description)
crs_text = unique(crs[,c("project_title", "text")])
names(crs_text) = c("Activity Name", "crs_text")
usaid_paf_short = merge(usaid_paf_short, crs_text, by="Activity Name", all.x=T)

usaid_paf_short = usaid_paf_short[,c(1:4,80,5:79)]
future_paf = subset(usaid_paf_short, !found_in_crs)
future_aa = subset(usaid_aa_short, !found_in_crs)
usaid_paf_short = subset(usaid_paf_short, found_in_crs)
usaid_paf_short$found_in_crs = NULL
usaid_aa_short = subset(usaid_aa_short, found_in_crs)
usaid_aa_short$found_in_crs = NULL

fwrite(usaid_paf_short, "output/usaid_new_paf.csv")
fwrite(usaid_aa_short, "output/usaid_new_aa.csv")

usaid_original = fread(file_location)
future_aa_original = subset(usaid_original, `Activity Name` %in% future_aa$`Activity Name`)
future_paf_original = subset(usaid_original, `Activity Name` %in% future_paf$`Activity Name`)
fwrite(future_aa, "output/usaid_noncrs_aa_text.csv")
fwrite(future_paf, "output/usaid_noncrs_paf_text.csv")
fwrite(future_aa_original, "output/usaid_noncrs_aa_data.csv")
fwrite(future_paf_original, "output/usaid_noncrs_paf_data.csv")