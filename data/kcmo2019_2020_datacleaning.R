#load library
library(dplyr)
library(reshape2)
library(openxlsx)
library(car)
library(rvest)

#######################################
# Data Cleaning
#######################################

#load data
raw_a = read.xlsx("data/Group3-311DataPreCOVID-Warm Season.xlsx")[,-31] %>% mutate(season = "PreCOVID-Warm")
raw_b = read.xlsx("data/Group2-311DataPreCOVID-Cold Season.xlsx")[,-31] %>% mutate(season = "PreCOVID-COld")
raw_c = read.xlsx("data/Group1-311DataDuringCOVID.xlsx")[,-31] %>% mutate(season = "COVID-warm")

raw = rbind(raw_a,raw_b)
raw = rbind(raw,raw_c)

sec = read.xlsx("data/SocioEconomic.xlsx")

rm(raw_a)
rm(raw_b)
rm(raw_c)

#merge
inc = sec %>% select(NEIGH = NBH_NAME, Income.Level)
dat = left_join(raw,inc, by="NEIGH")
dat$Income.Level[dat$NEIGH == "Central Blue Valley and Park Tower Gardens"] = "L"

#drop na neighborhood, income level, source, category, day to close
dat = dat %>% filter(!(is.na(NEIGH)|is.na(Income.Level)|is.na(SOURCE)|is.na(CATEGORY)|is.na(DAYTOCLOSE)))

#drop sept 2020 for low count
dat = dat %>% filter(!(CREATEYR==2020 & CREATEMO == 9))

#weight
dat$weight = 1

#######################################
# Scraping
#######################################
for (i in  1:length(dat$CASEURL)){
  
  tryCatch({
    #scrape description
    dat$description[i] = strsplit((read_html(diseasecontrol$CASEURL[1])%>% html_nodes("tr") %>% html_text()),"\r\n")[[10]][3]
    #remove double spaces
    dat$description[i] = gsub("  ","",toString(dat$description[i]))
    
    print(i)
  }, error=function(e){"ERROR"})
}

#save data for further analysis
save(dat, "data/kcmo2019_2020.rdata")

