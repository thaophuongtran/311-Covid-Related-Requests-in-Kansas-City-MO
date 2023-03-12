library(dplyr)
library(reshape2)
library(openxlsx)
library(car)

#change directory

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

diseasecontrol = dat %>% filter(REQTYPE == "Public Health-Disease Control-All")

library(rvest)

diseasecontrol$resol_sum = NA
diseasecontrol$resol_des = NA

diseasecontrol$resol_sum2 = NA
diseasecontrol$resol_des2 = NA

for (i in  1:length(diseasecontrol$CASEURL)){

  tryCatch({
    diseasecontrol$resol_sum[i] = strsplit((read_html(diseasecontrol$CASEURL[i])%>% html_nodes("tr") %>% html_text()),"\r\n")[[1]][5]
    diseasecontrol$resol_des[i] = strsplit((read_html(diseasecontrol$CASEURL[i])%>% html_nodes("tr") %>% html_text()),"\r\n")[[1]][7]

    diseasecontrol$resol_sum2[i] = gsub("  ","",toString(diseasecontrol$resol_sum[i]))
    diseasecontrol$resol_des2[i] = gsub("  ","",toString(diseasecontrol$resol_des[i]))

    print(i)
  }, error=function(e){"ERROR"})
}

#save data for futher analysis
save(dat, "C:/Users/bebut/Downloads/kcmo2019_2020.rdata")

