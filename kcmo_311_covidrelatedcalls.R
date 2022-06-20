# library(dplyr)
# library(reshape2)
# library(openxlsx)
# library(car)
# 
# #change directory
# 
# #######################################
# # Data Cleaning 
# #######################################
# 
# #load data
# raw_a = read.xlsx("Group3-311DataPreCOVID-Warm Season.xlsx")[,-31] %>% mutate(season = "PreCOVID-Warm")
# raw_b = read.xlsx("Group2-311DataPreCOVID-Cold Season.xlsx")[,-31] %>% mutate(season = "PreCOVID-COld")
# raw_c = read.xlsx("Group1-311DataDuringCOVID.xlsx")[,-31] %>% mutate(season = "COVID-warm")
# 
# raw = rbind(raw_a,raw_b) 
# raw = rbind(raw,raw_c)
# 
# sec = read.xlsx("SocioEconomic.xlsx")
# 
# rm(raw_a)
# rm(raw_b)
# rm(raw_c)
# 
# #merge 
# inc = sec %>% select(NEIGH = NBH_NAME, Income.Level)
# dat = left_join(raw,inc, by="NEIGH")
# dat$Income.Level[dat$NEIGH == "Central Blue Valley and Park Tower Gardens"] = "L"      
# 
# #drop na neighborhood, income level, source, category, day to close 
# dat = dat %>% filter(!(is.na(NEIGH)|is.na(Income.Level)|is.na(SOURCE)|is.na(CATEGORY)|is.na(DAYTOCLOSE)))
# 
# #drop sept 2020 for low count 
# dat = dat %>% filter(!(CREATEYR==2020 & CREATEMO == 9))
# 
# #weight 
# dat$weight = 1
# 
# #######################################
# # Scraping 
# #######################################
# 
# diseasecontrol = dat %>% filter(REQTYPE == "Public Health-Disease Control-All")
# 
# library(rvest)
# 
# diseasecontrol$resol_sum = NA
# diseasecontrol$resol_des = NA
# 
# diseasecontrol$resol_sum2 = NA
# diseasecontrol$resol_des2 = NA
# 
# for (i in  1:length(diseasecontrol$CASEURL)){
#   
#   tryCatch({
#     diseasecontrol$resol_sum[i] = strsplit((read_html(diseasecontrol$CASEURL[i])%>% html_nodes("tr") %>% html_text()),"\r\n")[[1]][5]
#     diseasecontrol$resol_des[i] = strsplit((read_html(diseasecontrol$CASEURL[i])%>% html_nodes("tr") %>% html_text()),"\r\n")[[1]][7]
#     
#     diseasecontrol$resol_sum2[i] = gsub("  ","",toString(diseasecontrol$resol_sum[i]))
#     diseasecontrol$resol_des2[i] = gsub("  ","",toString(diseasecontrol$resol_des[i]))
#     
#     print(i)
#   }, error=function(e){"ERROR"})
# }
# 
# save(dat, "C:/Users/bebut/Downloads/kcmo2019_2020.rdata")

#######################################
# Analysis 
#######################################

library(dplyr)
library(reshape2)
library(openxlsx)
library(car)
library(ggplot2)
library(hrbrthemes)
library(viridis)

load("kcmo2019_2020.rdata")

# create year+month var
dat$date = dat$CREATEYR*100 +dat$CREATEMO

dat$date = as.Date(as.character(dat$date*100+1),"%Y%m%d")

# create covid related calls 
dat = dat %>%
  mutate(
    
    covid = as.numeric(grepl("covid",tolower(description))|
                         grepl("corona",tolower(description))|
                         grepl("pandemic",tolower(description))|
                         grepl("virus",tolower(description))|
                         grepl("positive",tolower(description))
    ),
    
    mask = as.numeric(grepl("mask",tolower(description))|
                        grepl("face cover",tolower(description))|
                        grepl("ppe ",tolower(description))|
                        grepl("coverings",tolower(description))
    ),
    
    sdistance = as.numeric(grepl("social",tolower(description))|
                             grepl("distanc",tolower(description))|
                             grepl("6 feet",tolower(description))|
                             grepl("quarantine",tolower(description))|
                             grepl("stay at home",tolower(description))|
                             grepl("gathering",tolower(description))
    ),
    
    essential = as.numeric(grepl("essential",tolower(description))|
                             grepl("still open",tolower(description))|
                             grepl("open for business",tolower(description))|
                             grepl("open and operating",tolower(description))|
                             grepl("still operating",tolower(description))
                           ), 
    
    allcovid = as.numeric(covid==1|mask==1|sdistance==1|essential==1)
  )


###############################################################################
###############################################################################
#trends of each keyword
temp5 = dat %>% group_by(date) %>%
  summarize(
    
    covid = sum(grepl("covid",tolower(description))),
    corona = sum(grepl("corona",tolower(description))),
    pandemic = sum(grepl("pandemic",tolower(description))),
    virus = sum(grepl("virus",tolower(description))),
    positive = sum(grepl("positive",tolower(description))),
    
    mask = sum(grepl("mask",tolower(description))),
    face_cover = sum(grepl("face cover",tolower(description))),
    coverings = sum(grepl("coverings",tolower(description))),
    ppe = sum(grepl("ppe ",tolower(description))),
    
    social = sum(grepl("social",tolower(description))),
    distance = sum(grepl("distanc",tolower(description))),
    sixfeet = sum(grepl("6 feet",tolower(description))),
    
    quarantine = sum(grepl("quarantine",tolower(description))),
    stayathome = sum(grepl("stay at home",tolower(description))),
    gathering = sum(grepl("gathering",tolower(description))),
    
    essential = sum(grepl("essential",tolower(description))),
    open = sum(grepl("still open",tolower(description))),
    open_business = sum(grepl("open for business",tolower(description))),
    open_operate = sum(grepl("open and operating",tolower(description))),
    operate = sum(grepl("still operating",tolower(description))),
  ) 

temp5melt = melt(temp5,id.vars="date")
temp5melt$Keywords = temp5melt$variable
ggplot(temp5melt, aes(x=date,y = value, color = reorder(Keywords,-value)))+
  geom_line(lwd=1.50)+
  scale_color_viridis(discrete = TRUE) +
  labs(y="Number of requests",
      x="Date")+
  facet_wrap(~reorder(Keywords,-value),ncol=4,scale="free_y")+
  theme_bw()+
  theme(legend.position = "none")

###
# covid 
covid= read.csv("https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/COVID%20-%20City%20-%20Daily.csv")
covid = covid %>% filter(cityid == 37) %>%  #KCMO code is 37
                  select(year,month, day, new_case_count,new_death_count) #%>% group_by(year,month) %>% summarise_each(funs(mean(., na.rm = TRUE)))
covid$new_case_count = as.numeric(covid$new_case_count)
covid$new_death_count = as.numeric(covid$new_death_count)
covid = covid %>% group_by(year,month) %>% summarise_each(funs(mean(., na.rm = TRUE)))
covid$date = as.Date(as.character(covid$year*10000+covid$month*100+1),"%Y%m%d")
covid = covid %>% select(date,new_case_count,new_death_count)

#create total cases 
covid[is.na(covid)]=0
covid = covid %>%
  mutate(total_case_count = cumsum(new_case_count))

# trends 
tab_01 = dat %>% group_by(date) %>% summarise(
  #All_Requests = sum(weight,na.rm=T),
  COVID_19_related = sum(allcovid,na.rm=T),
  public_health = sum(CATEGORY=="Public Health",na.rm=T),
  public_safety = sum(CATEGORY=="Public Safety",na.rm=T),
  
) #%>% melt(id.vars="date")

tab_01 = tab_01 %>% left_join(covid,by="date")
tab_01[is.na(tab_01)] = 0


## plot covid-related words versus new cases
coef = 0.1
ggplot(tab_01[11:18,], aes(x=date))+
  geom_col(aes(y=COVID_19_related),lwd=1.50,fill="dodgerblue4")+
  geom_line(aes(y=new_case_count/coef),lwd=1.50,col="green3")+
  geom_line(aes(y=total_case_count/coef),lwd=1.50,col="red")+
  scale_y_continuous(name= "Number of requests",sec.axis = sec_axis(~.*coef,name="New cases"))+
  theme_bw() +
  labs(x="Date")
 
##############
dat_covid = dat %>% filter(season=="COVID-warm")

# theme of covid related requests 
tab_02 = dat_covid %>% 
  #group_by(CATEGORY) %>% 
  summarise(
  covid = sum(covid),
  mask = sum(mask),
  sdistance = sum(sdistance),
  essential = sum(essential),
  allcovid = sum(allcovid)
)
 
# theme of covid related requests by category
dat_covid %>% 
  filter(CATEGORY %in% c("Public Health","Public Safety","Parks & Recreation"))%>%
  group_by(CATEGORY) %>% 
  summarise(
    `COVID-19 Cases` = sum(covid),
    `Mask Mandate` = sum(mask),
    `Social Distance` = sum(sdistance),
    `Essential Business` = sum(essential)
  ) %>% melt() %>%
  ggplot(aes(x=reorder(variable,value), y = reorder(CATEGORY,value), fill = value))+
  geom_tile(show.legend = FALSE)+
  #geom_text(aes(label = paste(round(value*100),"%")),col="white") +
  geom_text(aes(label = value),col="white") +
  labs(y = "Category",x = "Covid-related requests",fill = "Share of requests")+
  theme(legend.position = "none")+
  theme_bw()


################
# contact method 

temp3 = dat_covid %>% 
  group_by(SOURCE) %>% 
  summarise(
    allcovid = sum(allcovid),
    noncovid = sum(weight)-allcovid,
  ) %>% mutate(sh_allcovid = allcovid/sum(allcovid)*100,
               sh_noncovid = noncovid/sum(noncovid)*100,)

################
# category 

temp4 = dat_covid %>% 
  group_by(REQTYPE) %>% 
  summarise(
    covid = sum(covid),
    mask = sum(mask),
    sdistance = sum(sdistance),
    essential = sum(essential),
    allcovid = sum(allcovid),
  )

temp4b = dat_covid %>% 
  group_by(CATEGORY) %>% 
  summarise(
    count = n(),
    covid = sum(covid),
    mask = sum(mask),
    sdistance = sum(sdistance),
    essential = sum(essential),
    allcovid = sum(allcovid),
  )


#comparing trends between covid-related requests and category
minmax <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

#scaled data
df <- dat %>% group_by(date) %>% summarise(
  #All_Requests = sum(weight,na.rm=T),
  COVID_19_related = sum(allcovid,na.rm=T),
  Public_Health = sum(CATEGORY=="Public Health",na.rm=T),
  Public_Safety = sum(CATEGORY=="Public Safety",na.rm=T),
  Parks_Recreation = sum(CATEGORY=="Parks & Recreation",na.rm=T),
  Property_Buildings = sum(CATEGORY=="Property / Buildings / Construction",na.rm=T),
  Trash_Recycling = sum(CATEGORY=="Trash / Recycling",na.rm=T),
) 
dfs1 <- as.data.frame(sapply(df[,-1], minmax)) %>% mutate(date = df$date)

#line plot comparing trends line
dfs1 %>% select(-COVID_19_related) %>% melt(id.vars="date") %>% ggplot() +
  geom_col(data = dfs1, mapping = aes(x = date, y = COVID_19_related),fill="dodgerblue4")+
  geom_line(aes(x = date, y = value, col = variable),lwd=1.25,alpha=0.8)+
  facet_grid(~variable)+
  labs(y = "Number of Requests (Min-Max Scale)", x = "Date")+
  theme_bw()+
  theme(legend.position = "none")

#share of covid related requests by category and date
dat_covid %>% 
  filter(CATEGORY %in% c("Public Health","Public Safety","Parks & Recreation","Property / Buildings / Construction","Trash / Recycling"))%>%
  group_by(date,CATEGORY) %>% 
  summarize(
    n= n(),
    covid_sh = mean(allcovid),
    covid_ct = sum(allcovid)
  ) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_col(fill="dodgerblue4",alpha=0.2)+
  geom_col(aes(y=covid_ct),fill="dodgerblue4")+
  facet_wrap(~reorder(CATEGORY,n),scale="free_y",ncol=5)+
  labs(y="Number of Requests",x="Date")+
  theme_bw()

# no date breakdown
dat_covid %>% 
  filter(CATEGORY %in% c("Public Health","Public Safety","Parks & Recreation","Property / Buildings / Construction","Trash / Recycling"))%>%
  group_by(CATEGORY) %>% 
  summarize(
    n= n(),
    covid_sh = mean(allcovid),
    covid_ct = sum(allcovid)
  ) %>% 
  ggplot(aes(x =n, y = reorder(CATEGORY,-n))) +
  geom_col(fill="dodgerblue4",alpha=0.2)+
  geom_col(aes(x=covid_ct),fill="dodgerblue4")+
  geom_text(aes(label = paste(round(covid_sh*100,1),"%")),hjust=-1)+
  labs(y="",x="Number of Requests")+
  theme_bw()

#share of covid related requests in changes of category
#y/y chg
diff = dat %>% 
  filter(CATEGORY %in% c("Public Health","Public Safety","Parks & Recreation","Property / Buildings / Construction","Trash / Recycling")) %>%
  group_by(CATEGORY,CREATEYR,CREATEMO) %>% 
  count() %>% 
  dcast(CATEGORY+CREATEMO~CREATEYR) %>% 
  mutate(diff = `2020`-`2019`)%>% 
  na.omit()  
  
#covid related requests
diffcovid = dat_covid %>% 
  filter(CATEGORY %in% c("Public Health","Public Safety","Parks & Recreation","Property / Buildings / Construction","Trash / Recycling")) %>%
  group_by(CATEGORY,CREATEMO,date) %>%
  summarize(covid= sum(allcovid)) 

#plot by category and date
diffcovid %>% inner_join(diff) %>% 
  ggplot(aes(y =diff, x = date)) +
  geom_col(fill="dodgerblue4",alpha=0.2)+
  geom_col(aes(y=covid),fill="dodgerblue4")+
  facet_wrap(~reorder(CATEGORY,covid),scale="free_y",ncol=3)+
  labs(y="Year-over-year Change In Number of Requests",x="Date")+
  theme_bw()

#plot by category
diffcovid %>% inner_join(diff) %>% select(-c(date,CREATEMO)) %>%group_by(CATEGORY) %>%summarise_each(sum) %>%  
  ggplot(aes(x =diff, y = reorder(CATEGORY,covid))) +
  geom_col(fill="dodgerblue4",alpha=0.2)+
  geom_col(aes(x=covid),fill="dodgerblue4")+
  geom_text(aes(label = paste(round(covid/diff*100),"%")),hjust=-1)+
  labs(y="",x="Year-over-year Change In Number of Requests")+
  xlim(c(0,5000))+
  theme_bw()

#plot request volume by category
dat %>% 
  filter(season != "PreCOVID-COld") %>%
  group_by(season,CREATEMO) %>% 
  count() %>%
  arrange(desc(season)) %>%
  mutate(season = ifelse(season == "PreCOVID-Warm","2019","2020"))%>%
  ggplot(aes(x = CREATEMO, y = n,group=reorder(season,-n),fill=reorder(season,-n) ))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("dodgerblue4","green3"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Number of requests", x = "Month",fill= "")+
  ylim(c(0,15000))+
  theme_bw()+
  theme(legend.position = c(0.9,0.9),
        legend.background = element_blank())





