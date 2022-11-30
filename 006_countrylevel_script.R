rm(list=ls())
require(dplyr)
require(tidyr)
require(ggplot2)
require(mgsub)
require(countrycode)
require(gridExtra)
options(scipen=999)
library(rvest)
library(mgcv)
library(wbstats)
library(cepiigeodist)



# As well as Emma's prior filters...
# Data adjustments (manual):
# Merged all continental origins into single column
# Merged all continental destinations into single column -- standardised
# column names with origins (CA = NAm; OC-PI = OC; Ant-Sub = A-S)
# Added several blank, non-pathogen origins using Google Sheet entries (Diuraphis noxia = AS;
# Ephestia kuehniella = AS; Rumex lunaria = EUR; Thaumetopoea processionea = EUR)

### PREP by continent

data<-read.csv("data/invacost_origin_expanded_fourpointone_DN2.csv") # continent column manually fixed by Dat Nguyen

data$origin<-NA
for (i in 1:nrow(data))
{
  data$origin[i]<-paste(colnames(data)[33:38][which(data[i,33:38]==1)], collapse=";")
}

data<-data[,c(1:32, 39:268, 282:285)]
data<-data[,-(which(colnames(data)=="NA."))]
data$origin_code<-"NA"
for (i in 1:nrow(data))
{
  if(length(which(data[i,]==1))>0)
  {
    data$origin_code[i]<-paste(colnames(data)[which(data[i,33:261]==1)+32],collapse=';')
  }
}
data$destin_code<-sapply(data$Official_country,FUN=function(x){paste(countrycode(unlist(strsplit(x, '/')),'country.name', 'iso3c'), collapse=';')})
data <- data[!(data$origin_code==""),] # blank origins (i.e. pathogens) omitted
data <- data[!((data$origin_code)=="NA"),] # blank origins (i.e. pathogens) omitted

data <- data[!is.na(data$Cost_estimate_per_year_2017_USD_exchange_rate),] # blank costs removed

data$mil<-data$Cost_estimate_per_year_2017_USD_exchange_rate/1000000
sum(data$mil) # aggregate cost (US$ millions)

data$TenYear<-signif(data$Impact_year,digits=3)
data<-subset(data, (TenYear>=1960&TenYear<2020))

domesticated<-c('Felis catus', 'Canis lupus', 'Ovis aries', 'Camelus dromedarius','Sus scrofa','Equus caballus','Equus asinus', 'Mustela furo','Capra hircus', "Bos taurus")
domesticated[which(domesticated%in%data$Species==F)]
data<-subset(data, Species%in%domesticated==F)
length(unique(data$Reference_ID))
length(unique(data$Species))

data$N <- 1:nrow(data) # unique identifier for qualification below
# data$n_continent<-rowSums(data[,33:38])
# data$n_country<-rowSums(data[,39:267])
# length(unique(data$Species[which(data$n_continent>data$n_country)]))
# Dividing costs among multiple origins

expanded<-data %>% 
  mutate(origin_code = strsplit(as.character(origin_code), ";")) %>% 
  unnest(origin_code) # entries expanded per origin

expanded<-expanded %>% 
  mutate(destin_code = strsplit(as.character(destin_code), ";")) %>% 
  unnest(destin_code) # entries expanded again per destination

expanded<-expanded %>%
  group_by(N) %>%
  mutate(mil = mil / n()) # cost qualified by #origins and #destinations using 'N' qualifier

expanded<-subset(expanded, destin_code!='NA')
expanded<-subset(expanded, origin_code!='NA')

# we do not distinguish invaders from hong kong, taiwan and macau to those from china
expanded$origin_code<-gsub("MAC", "CHN", expanded$origin_code)
expanded$origin_code<-gsub("HKG", "CHN", expanded$origin_code)
expanded$origin_code<-gsub("TWN", "CHN", expanded$origin_code)

expanded$destin_code<-gsub("MAC", "CHN", expanded$destin_code)
expanded$destin_code<-gsub("HKG", "CHN", expanded$destin_code)
expanded$destin_code<-gsub("TWN", "CHN", expanded$destin_code)


length(unique(expanded$Species))
length(unique(expanded$Cost_ID))
length(unique(expanded$Reference_ID))
length(unique(expanded$N))
length(unique(expanded$origin_code))
length(unique(expanded$destin_code))

### BASIC ANALYSES

receivers<-aggregate(mil~destin_code,data=expanded,FUN=sum)
receive_num<-expanded%>%group_by(destin_code)%>%summarize_at('Reference_ID', n_distinct)
receivers$mil_per_pub<-receivers$mil/receive_num$Reference_ID
givers<-aggregate(mil~origin_code,data=expanded,FUN=sum)
givers_num<-expanded%>%group_by(origin_code)%>%summarize_at('Reference_ID', n_distinct)
givers$mil_per_pub<-givers$mil/givers_num$Reference_ID
top_pairs<-aggregate(mil~origin_code*destin_code,data=expanded,FUN=sum, na.rm=T)
top_num<-expanded%>%group_by(origin_code, destin_code)%>%summarize_at('Reference_ID', n_distinct)
top_pairs$mil_per_pub<-top_pairs$mil/top_num$Reference_ID

saveRDS(receivers, 'output/receivers.RDS')
saveRDS(givers, 'output/givers.RDS')
saveRDS(top_pairs, 'output/top_pairs.RDS')

# receivers_missing<-receivers[order(receivers$mil, decreasing=T)[1:10],]
# receivers_missing$Country<-countrycode(receivers_missing$destin_code,'iso3c','country.name')
# pdf('../output/Missing_receivers.pdf')
# ggplot(data=receivers_missing, aes(x=reorder(Country,mil), y=mil))+
#   coord_flip()+
#   geom_bar(stat="identity")+theme_classic()+ylab("Top 10 receivers (millions US$)")+
#   xlab(label="")+
#   theme(axis.ticks=element_blank(), axis.text.x=element_text(size=10),legend.position = "none")
# dev.off()

flow2<-aggregate(mil~origin_code*destin_code+Species,data=expanded,FUN=sum)
flow2<-flow2 %>%
  mutate(N = 1) # qualifying factor
flow3<-flow2 %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination
byspp<-aggregate(N~origin_code*destin_code,data=flow3,FUN=sum)
colnames(byspp)[3]<-"num"
aggregate(num~destin_code,data=byspp,FUN=sum)
aggregate(num~origin_code,data=byspp,FUN=sum)
aggregate(num~origin_code*destin_code,data=byspp,FUN=sum)



df<-aggregate(mil~origin_code*destin_code+Species+TenYear,data=expanded,FUN=sum)
df<-df %>%
  mutate(N = 1) # qualifying factor 
df<-df %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination

Give <- expanded %>% group_by(origin_code, TenYear) %>% summarise(cost=sum(mil))
Take <- expanded %>% group_by(destin_code, TenYear) %>% summarise(cost=sum(mil))
Give_spp <- df %>% group_by(origin_code, TenYear) %>% summarise(spp=sum(N))
Take_spp <- df %>% group_by(destin_code, TenYear) %>% summarise(spp=sum(N))

# Net costs
names(receivers)[names(receivers) == 'destin_code'] <- 'country'
names(givers)[names(givers) == 'origin_code'] <- 'country'
net_costs<-merge(givers, receivers, by = c("country"), all.x = TRUE)
net_costs$net <- (net_costs$mil.x - ifelse(is.na(net_costs$mil.y),0,net_costs$mil.y))
net_costs<-net_costs[order(net_costs$net), ]
net_costs 

table(sign(net_costs$net)) # Negative values = net receiver; Positive values = net sender

sum(is.na(net_costs$mil.y))  # Number of countries that send costs (mil.x) but don't receive them (mil.y)

# To reverse column name change so rest of script works
names(receivers)[names(receivers) == 'country'] <- 'destin_code'
names(givers)[names(givers) == 'country'] <- 'origin_code'

##link with sTwist first record database

stwist<-read.table('~/Downloads/SInAS_AlienSpeciesDB_2.4.1.csv', header=T)

colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$eventDate<-gsub(";.*","",stwist$eventDate )
stwist$eventDate<-as.numeric(stwist$eventDate) # use minimum first record date

#Assign iso3c codes#
stwist$code<-countrycode(stwist$Official_country, 'country.name', 'iso3c')
continent<-read.csv('data/country-origin-ref_4p1_DN.csv')
continent$code<-countrycode(continent$Official_country, 'country.name', 'iso3c')
stwist$Destin_cont<-continent$Geographic_region2[match(stwist$code,continent$code)]

stwist$TenYear<-signif(stwist$eventDate, digits=3)
stwist<-subset(stwist, eventDate>1500)#use post-colonial invasions
domesticated<-c('Felis catus', 'Canis lupus', 'Ovis aries', 'Camelus dromedarius','Sus scrofa','Equus caballus','Equus asinus', 'Mustela furo','Capra hircus')
stwist<-subset(stwist, stwist$scientificName%in%domesticated==F)

stwist_cont<-aggregate(locationID~Destin_cont+Species+TenYear,data=stwist,FUN=length)
stwist_cont<-subset(stwist_cont, Species%in%expanded$Species)
stwist_cont<-stwist_cont %>%
  mutate(N = 1) # qualifying factor 
stwist_cont<-stwist_cont %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination

socioeco_dat<-readRDS('data/soc_econ_country.rds') # From Sardain, Leung et al. Nature Sustainability
socioeco_dat<-subset(socioeco_dat, IHS.i%in%countrycode(unique(expanded$destin_code), 'iso3c', 'country.name'))
socioeco_dat<-subset(socioeco_dat, IHS.j%in%countrycode(unique(expanded$destin_code), 'iso3c', 'country.name'))


###trade data

# alldata<-read.csv("invacost_origin_expanded_DN.csv") # continent column manually fixed by Dat Nguyen
# origin_countries<-colnames(alldata)[38:267]
# destin_countries<-countrycode(unique(data$Official_country), 'country.name', 'iso3c')
# trade<-matrix(0, length(origin_countries),length(destin_countries))
#  baci_country<-read.csv('~/Downloads/country_codes_V202102.csv')
# # 
#  baci_dat<-read.csv(paste0("~/Downloads/BACI_HS92_V202102.csv"))
#  baci_dat_2015<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y2015_V202102.csv"))
#  baci_dat_2016<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y2016_V202102.csv"))
#  baci_dat_2017<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y2017_V202102.csv"))
#  baci_dat_2018<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y2018_V202102.csv"))
# 
# 
# for (j in 1:length(destin_countries))
# {
#   for (i in 1:length(origin_countries))
#   {
#     qq<-baci_dat[which(baci_dat$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     qq_2016<-baci_dat_2016[which(baci_dat_2016$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2016$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     qq_2017<-baci_dat_2017[which(baci_dat_2017$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2017$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     qq_2015<-baci_dat_2015[which(baci_dat_2015$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2015$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     qq_2018<-baci_dat_2018[which(baci_dat_2018$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2018$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     if(nrow(qq)>0|nrow(qq_2015)>0|nrow(qq_2016)>0|nrow(qq_2017)>0|nrow(qq_2018)>0)
#     {trade[i,j]<-trade[i,j]+0.2*(sum(qq$q, na.rm=T)+sum(qq_2015$q, na.rm=T)+sum(qq_2016$q, na.rm=T)+sum(qq_2017$q, na.rm=T)+sum(qq_2018$q, na.rm=T))}
#   }
# }
# saveRDS(trade, "../output/trade_averaged.RDS")
#  trade_historical<-matrix(0, length(origin_countries),length(destin_countries))
#  baci_dat<-read.csv(paste0("~/Downloads/BACI_HS92_V202102.csv"))
#  baci_dat_2015<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y1996_V202102.csv"))
#  baci_dat_2016<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y1997_V202102.csv"))
#  baci_dat_2017<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y1998_V202102.csv"))
#  baci_dat_2018<-read.csv(paste0("~/Downloads/BACI_HS92_V202102/BACI_HS92_Y1999_V202102.csv"))
#  
#  
#  for (j in 1:length(destin_countries))
#  {
#    for (i in 1:length(origin_countries))
#    {
#      qq<-baci_dat[which(baci_dat$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#      qq_2016<-baci_dat_2016[which(baci_dat_2016$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2016$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#      qq_2017<-baci_dat_2017[which(baci_dat_2017$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2017$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#      qq_2015<-baci_dat_2015[which(baci_dat_2015$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2015$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#      qq_2018<-baci_dat_2018[which(baci_dat_2018$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat_2018$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#      if(nrow(qq)>0|nrow(qq_2015)>0|nrow(qq_2016)>0|nrow(qq_2017)>0|nrow(qq_2018)>0)
#      {trade_historical[i,j]<-trade_historical[i,j]+0.2*(sum(qq$q, na.rm=T)+sum(qq_2015$q, na.rm=T)+sum(qq_2016$q, na.rm=T)+sum(qq_2017$q, na.rm=T)+sum(qq_2018$q, na.rm=T))}
#    }
#  }
# saveRDS(trade_historical, "../output/trade_historical_averaged.RDS")
trade_historical<-readRDS('../biosecurity_invacost/data/trade_historical_averaged.RDS')
trade<-readRDS('../biosecurity_invacost/data/trade_averaged.RDS')
trade<-trade[-163,]
colnames(socioeco_dat)[15:16]<-c('origin_code', 'destin_code')
socioeco_dat$origin_code<-countrycode(socioeco_dat$origin_code,'country.name', 'iso3c')
socioeco_dat$destin_code<-countrycode(socioeco_dat$destin_code,'country.name', 'iso3c')
socioeco_dat<-socioeco_dat%>%group_by(origin_code, destin_code)%>%summarize_at(c( 'FTA', "CCH", "CL", "CB"), max)
data(dist_cepii)
colnames(dist_cepii)[1:2]<-c('origin_code', 'destin_code')
socioeco_dat<-merge(socioeco_dat,dist_cepii, by=c('origin_code', 'destin_code'), all.y=T)
socioeco_dat<-socioeco_dat[,c(1:3,7,9,11,15)]
colnames(socioeco_dat)[4:7]<-c("CB", "CL", "CCH", "Distance")
expanded<-left_join(expanded, socioeco_dat, by=c('origin_code', 'destin_code'))
colnames(socioeco_dat)[1:2]<-colnames(socioeco_dat)[2:1]
gravity<-readRDS('data/Gravity_V202102.Rds')
colnames(gravity)
gravity<-gravity[,c(2:3,63)]
gravity<-gravity%>%group_by(iso3_o, iso3_d)%>%summarize_at('rta',max, na.rm=T)
colnames(gravity)[1:2]<-c('origin_code', 'destin_code')
expanded<-expanded[,-271]
expanded<-left_join(expanded, gravity, by=c('origin_code', 'destin_code'))
colnames(expanded)[275]<-c("FTA")
expanded$FTA[which(is.na(expanded$FTA))]<-0
expanded$CCH[which(is.na(expanded$CCH))]<-0
expanded$CB[which(is.na(expanded$CB))]<-0
expanded$CL[which(is.na(expanded$CL))]<-0
length(unique(paste(expanded$origin_code[which(is.na(expanded$Distance))], expanded$destin_code[which(is.na(expanded$Distance))])))

expanded$Distance[which(is.na(expanded$Distance))]<-mean(expanded$Distance, na.rm=T)

origin_countries<-colnames(data)[33:261]
destin_countries<-countrycode(unique(data$Official_country), 'country.name', 'iso3c')

### check out top trade associations
top10pairs<-arrayInd(order(trade, decreasing=T)[1:10], dim(trade))

top10pairs[,1]<-rownames(trade)[top10pairs[,1]]
top10pairs[,2]<-colnames(trade)[as.numeric(top10pairs[,2])]
colnames(top10pairs)<-c("From", 'To')
top10pairs<-as.data.frame(top10pairs)
top10pairs$Vol<-unlist(trade[order(trade, decreasing=T)[1:10]])

colnames(trade_historical)<-colnames(trade)[1:92]
rownames(trade_historical)<-rownames(trade)
historical_pairs<-arrayInd(order(trade_historical, decreasing=T)[1:10], dim(trade_historical))

historical_pairs[,1]<-rownames(trade)[historical_pairs[,1]]
historical_pairs[,2]<-colnames(trade)[as.numeric(historical_pairs[,2])]
colnames(historical_pairs)<-c("From", 'To')
historical_pairs<-as.data.frame(historical_pairs)
historical_pairs$Vol<-unlist(trade_historical[order(trade_historical, decreasing=T)[1:10]])


expanded<-expanded %>%
  group_by(N) %>%
  mutate(mil2 = mil * n()) # cost qualified by #origins and #destinations using 'N' qualifier

### Model cost donation/reception
alldata<-expanded%>%group_by(origin_code, destin_code, TenYear)%>%summarise_at(c('mil'),sum)
alldata<-merge(alldata, byspp, all.x=T)

alldata2<-cbind(expanded)%>%group_by(origin_code, destin_code, TenYear)%>%summarise_at(c('Distance'),mean)

alldata3<-cbind(expanded)%>%group_by(origin_code, destin_code, TenYear)%>%summarise_at(c('CB', 'CL', 'CCH', 'FTA'),max)
alldata<-cbind(alldata, alldata2[,4], alldata3[,4:7])
colnames(alldata)[5]<-'n_spp'
alldata$trade<-0
alldata$trade_historical<-0
for (i in 1:nrow(alldata))
{
  orig<-alldata$origin_code[i]
  dest<-alldata$destin_code[i]
  alldata$trade[i]<-sum(trade[which(origin_countries%in%orig), which(destin_countries==dest)],na.rm=T)
  alldata$trade_historical[i]<-sum(trade_historical[which(origin_countries%in%orig), which(destin_countries==dest)],na.rm=T)
}

##species richness by country
sr<-read_html('https://rainforests.mongabay.com/03highest_biodiversity.htm')
sr_tab<-html_table(sr)[[1]]
sr_tab<-as.data.frame(sr_tab)
for (i in 2:7)
{
  sr_tab[,i]<-gsub(",", "",sr_tab[,i])
  
  sr_tab[,i]<-as.numeric(sr_tab[,i])
}
sr_tab$code<-countrycode(sr_tab$Country, 'country.name', 'iso3c')
sr_tab$tot_sr<-rowSums(sr_tab[,2:7], na.rm=T)
alldata$sr_orig<-sr_tab$tot_sr[match(alldata$origin_code,sr_tab$code)]
alldata$sr_dest<-sr_tab$tot_sr[match(alldata$destin_code,sr_tab$code)]
length(which(is.na(alldata$sr_orig))) #96
alldata$sr_orig[which(is.na(alldata$sr_orig))]<-mean(alldata$sr_orig, na.rm=T)
length(which(is.na(alldata$sr_dest))) #42
alldata$sr_dest[which(is.na(alldata$sr_dest))]<-mean(alldata$sr_dest, na.rm=T)
given<-alldata%>%group_by(origin_code, TenYear)%>%summarize_at('mil', sum, na.rm=T)
colnames(given)[3]<-"totalgivenTenYear"
alldata<-merge(alldata, given, by=c("origin_code", "TenYear"))

area<-wb_data("AG.SRF.TOTL.K2", country=unique(alldata$destin_code), start_date = 2018, end_date=2018)
area<-area[,c('iso3c','AG.SRF.TOTL.K2')]
colnames(area)[1]<-'destin_code'
alldata<-merge(alldata, area, by='destin_code')
colnames(alldata)[16]<-'area.j'
area<-wb_data("AG.SRF.TOTL.K2", country=unique(alldata$origin_code), start_date = 2018, end_date=2018)
area<-area[,c('iso3c','AG.SRF.TOTL.K2')]
colnames(area)[1]<-'origin_code'
alldata<-merge(alldata, area, by='origin_code', all.x=T)
colnames(alldata)[17]<-'area.i'
length(which(is.na(alldata$area.i)))
alldata$area.i[which(is.na(alldata$area.i))]<-mean(alldata$area.i, na.rm=T)
length(which(is.na(alldata$area.j)))
alldata$area.j[which(is.na(alldata$area.j))]<-mean(alldata$area.j, na.rm=T)


pop_decade<-wb_data("SP.POP.TOTL", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
pop_decade$TenYear<-signif(pop_decade$date,3)
pop2<-pop_decade%>%group_by(iso3c, TenYear)%>%summarize_at('SP.POP.TOTL', mean, na.rm=T)
colnames(pop2)[1]<-'destin_code'
alldata<-merge(alldata, pop2, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[18]<-'pop.j'
pop_decade<-wb_data("SP.POP.TOTL", country=unique(alldata$origin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
pop_decade$TenYear<-signif(pop_decade$date,3)
pop2<-pop_decade%>%group_by(iso3c, TenYear)%>%summarize_at('SP.POP.TOTL', mean, na.rm=T)
colnames(pop2)[1]<-'origin_code'
alldata<-merge(alldata, pop2, by=c('origin_code', "TenYear"), all.x=T)
colnames(alldata)[19]<-'pop.i'


gdp_decade<-wb_data("NY.GDP.MKTP.CD", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
gdp_decade$TenYear<-signif(gdp_decade$date,3)
gdp2<-gdp_decade%>%group_by(iso3c, TenYear)%>%summarize_at('NY.GDP.MKTP.CD', mean, na.rm=T)
colnames(gdp2)[1]<-'destin_code'
alldata<-merge(alldata, gdp2, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[20]<-'gdp.j'
gdp_decade<-wb_data("NY.GDP.MKTP.CD", country=unique(alldata$origin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
gdp_decade$TenYear<-signif(gdp_decade$date,3)
gdp2<-gdp_decade%>%group_by(iso3c, TenYear)%>%summarize_at('NY.GDP.MKTP.CD', mean, na.rm=T)
colnames(gdp2)[1]<-'origin_code'
alldata<-merge(alldata, gdp2, by=c('origin_code', "TenYear"), all.x=T)
colnames(alldata)[21]<-'gdp.i'

n_refs<-expanded%>%group_by(TenYear, destin_code)%>%summarize_at("Reference_ID", n_distinct)
alldata<-merge(alldata, n_refs)
n_refs<-expanded%>%group_by(TenYear, origin_code)%>%summarize_at("Reference_ID", n_distinct)
colnames(n_refs)[3]<-'Reference_ID_origin'
alldata<-merge(alldata, n_refs)


cap_form<-wb_data("NE.GDI.TOTL.CD", country=unique(alldata$origin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
cap_form$TenYear<-signif(cap_form$date,3)
cap_form<-cap_form%>%group_by(iso3c, TenYear)%>%summarize_at('NE.GDI.TOTL.CD', mean, na.rm=T)
colnames(cap_form)[1]<-'origin_code'
alldata<-merge(alldata, cap_form, by=c('origin_code', "TenYear"), all.x=T)
colnames(alldata)[24]<-'cap_form.i'

cap_form<-wb_data("NE.GDI.TOTL.CD", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
cap_form$TenYear<-signif(cap_form$date,3)
cap_form<-cap_form%>%group_by(iso3c, TenYear)%>%summarize_at('NE.GDI.TOTL.CD', mean, na.rm=T)
colnames(cap_form)[1]<-'destin_code'
alldata<-merge(alldata, cap_form, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[25]<-'cap_form.j'


tourism_rec<-wb_data("ST.INT.RCPT.CD", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
tourism_rec$TenYear<-signif(tourism_rec$date,3)
tourism_rec<-tourism_rec%>%group_by(iso3c, TenYear)%>%summarize_at('ST.INT.RCPT.CD', mean, na.rm=T)
colnames(tourism_rec)[1]<-'destin_code'
alldata<-merge(alldata, tourism_rec, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[26]<-'tourism_rec'

tourism_exp<-wb_data("ST.INT.XPND.CD", country=unique(alldata$origin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
tourism_exp$TenYear<-signif(tourism_exp$date,3)
tourism_exp<-tourism_exp%>%group_by(iso3c, TenYear)%>%summarize_at('ST.INT.XPND.CD', mean, na.rm=T)
colnames(tourism_exp)[1]<-'origin_code'
alldata<-merge(alldata, tourism_exp, by=c('origin_code', "TenYear"), all.x=T)
colnames(alldata)[27]<-'tourism_exp'


rd_dens.i<-wb_data("IS.ROD.DNST.K2", country=unique(alldata$origin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
rd_dens.i$TenYear<-signif(rd_dens.i$date,3)
rd_dens.i<-rd_dens.i%>%group_by(iso3c, TenYear)%>%summarize_at('IS.ROD.DNST.K2', mean, na.rm=T)
colnames(rd_dens.i)[1]<-'origin_code'
alldata<-merge(alldata, rd_dens.i, by=c('origin_code', "TenYear"), all.x=T)
colnames(alldata)[28]<-'rd_dens.i'

rd_dens.j<-wb_data("IS.ROD.DNST.K2", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
rd_dens.j$TenYear<-signif(rd_dens.j$date,3)
rd_dens.j<-rd_dens.j%>%group_by(iso3c, TenYear)%>%summarize_at('IS.ROD.DNST.K2', mean, na.rm=T)
colnames(rd_dens.j)[1]<-'destin_code'
alldata<-merge(alldata, rd_dens.j, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[29]<-'rd_dens.j'

aff.j<-wb_data("NV.AGR.TOTL.ZS", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
aff.j$TenYear<-signif(aff.j$date,3)
aff.j<-aff.j%>%group_by(iso3c, TenYear)%>%summarize_at('NV.AGR.TOTL.ZS', mean, na.rm=T)
colnames(aff.j)[1]<-'destin_code'
alldata<-merge(alldata, aff.j, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[30]<-'aff.j'

aff.i<-wb_data("NV.AGR.TOTL.ZS", country=unique(alldata$origin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
aff.i$TenYear<-signif(aff.i$date,3)
aff.i<-aff.i%>%group_by(iso3c, TenYear)%>%summarize_at('NV.AGR.TOTL.ZS', mean, na.rm=T)
colnames(aff.i)[1]<-'origin_code'
alldata<-merge(alldata, aff.i, by=c('origin_code', "TenYear"), all.x=T)
colnames(alldata)[31]<-'aff.i'

food_imports<-wb_data("TM.VAL.FOOD.ZS.UN", country=unique(alldata$destin_code), start_date = min(alldata$TenYear), end_date=(max(alldata$TenYear)+9))
food_imports$TenYear<-signif(food_imports$date,3)
food_imports<-food_imports%>%group_by(iso3c, TenYear)%>%summarize_at('TM.VAL.FOOD.ZS.UN', mean, na.rm=T)
colnames(food_imports)[1]<-'destin_code'
alldata<-merge(alldata, food_imports, by=c('destin_code', "TenYear"), all.x=T)
colnames(alldata)[32]<-'food_imports'

length(unique(alldata$origin_code[which(is.nan(alldata$pop.i))]))#1
length(unique(alldata$origin_code[which(is.nan(alldata$gdp.i))]))#66
length(unique(alldata$origin_code[which(is.nan(alldata$cap_form.i))]))#102
length(unique(alldata$destin_code[which(is.nan(alldata$cap_form.j))]))#4
length(unique(alldata$origin_code[which(is.nan(alldata$aff.i))]))#116
length(unique(alldata$destin_code[which(is.nan(alldata$aff.j))]))#9
length(unique(alldata$origin_code[which(is.nan(alldata$tourism_exp))]))#201
length(unique(alldata$destin_code[which(is.nan(alldata$tourism_rec))]))#28
length(unique(alldata$origin_code[which(is.nan(alldata$rd_dens.i))]))#201
length(unique(alldata$destin_code[which(is.nan(alldata$rd_dens.j))]))#28
length(unique(alldata$destin_code[which(is.nan(alldata$food_imports))]))#2



for (i in 1:nrow(alldata))
{
  if (is.nan(alldata$pop.i[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$pop.i)==F)
    alldata$pop.i[i]<-sub$pop.i[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$pop.j[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$pop.j)==F)
    alldata$pop.j[i]<-sub$pop.j[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$gdp.i[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$gdp.i)==F)
    if (nrow(sub)>0)
    {
    alldata$gdp.i[i]<-sub$gdp.i[which.min(abs(sub$TenYear-alldata$TenYear[i]))]}
  }
  if (is.nan(alldata$gdp.j[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$gdp.j)==F)
    alldata$gdp.j[i]<-sub$gdp.j[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$cap_form.i[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]&is.nan(alldata$cap_form.i)==F)
    if (nrow(sub)>0){alldata$cap_form.i[i]<-sub$cap_form.i[which.min(abs(sub$TenYear-alldata$TenYear[i]))]}
  }
  if (is.nan(alldata$cap_form.j[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$cap_form.j)==F)
    if (nrow(sub)>0){alldata$cap_form.j[i]<-sub$cap_form.j[which.min(abs(sub$TenYear-alldata$TenYear[i]))]}
  }
  if (is.nan(alldata$tourism_rec[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$tourism_rec)==F)
    if (nrow(sub)>0)alldata$tourism_rec[i]<-sub$tourism_rec[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$tourism_exp[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$tourism_exp)==F)
   if (nrow(sub)>0) alldata$tourism_exp[i]<-sub$tourism_exp[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$rd_dens.i[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$rd_dens.i)==F)
    if (nrow(sub)>0)alldata$rd_dens.i[i]<-sub$rd_dens.i[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$rd_dens.j[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$rd_dens.j)==F)
   if (nrow(sub)>0) alldata$rd_dens.j[i]<-sub$rd_dens.j[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$aff.i[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$aff.i)==F)
    if (nrow(sub)>0)alldata$aff.i[i]<-sub$aff.i[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$aff.j[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$aff.j)==F)
    if (nrow(sub)>0)alldata$aff.j[i]<-sub$aff.j[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }

  if (is.nan(alldata$food_imports[i]))
  {
    sub<-subset(alldata, destin_code==alldata$destin_code[i]& is.nan(alldata$food_imports)==F)
    if (nrow(sub)>0)alldata$food_imports[i]<-sub$food_imports[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
}

library(rworldmap)
data("countriesCoarse")
countrylat<-cbind(as.character(countriesCoarse$ISO3), countriesCoarse$LAT)
colnames(countrylat)<-c("origin_code", "lat")
alldata<-merge(alldata, countrylat, all.x=T)
colnames(alldata)[33]<-'lat.i'
colnames(countrylat)<-c("destin_code", "lat")
alldata<-merge(alldata, countrylat, all.x=T)
colnames(alldata)[34]<-'lat.j'

alldata<-subset(alldata, origin_code%in%c("GIB", "VGB", 'GLP','MTQ','MSR','GUF','ESH','BES','AIA','SPM','NFK','CXR','COK','CCK','SJM','PCN','GGY','JEY')==F)# removing territories not considered countries by world bank
length(unique(alldata$origin_code[(which(is.na(alldata$gdp.i)))]))
length(unique(alldata$origin_code[(which(is.na(alldata$cap_form.i)))]))
length(unique(alldata$destin_code[(which(is.na(alldata$cap_form.j)))]))
length(unique(alldata$origin_code[(which(is.na(alldata$aff.i)))]))
length(unique(alldata$destin_code[(which(is.na(alldata$aff.j)))]))
length(unique(alldata$origin_code[(which(is.na(alldata$tourism_exp)))]))
length(unique(alldata$destin_code[(which(is.na(alldata$tourism_rec)))]))
length(unique(alldata$origin_code[(which(is.na(alldata$rd_dens.i)))]))
length(unique(alldata$destin_code[(which(is.na(alldata$rd_dens.j)))]))
length(unique(alldata$destin_code[(which(is.na(alldata$food_imports)))]))


alldata$gdp.i[which(is.na(alldata$gdp.i))]<-mean(alldata$gdp.i, na.rm=T)
alldata$pop.i[which(is.na(alldata$pop.i))]<-mean(alldata$pop.i, na.rm=T)

length(unique(c(alldata$origin_code,alldata$destin_code)))
alldata$origin_code<-as.factor(alldata$origin_code)
alldata$destin_code<-as.factor(alldata$destin_code)
#levels(alldata$destin_code)<-levels(alldata$origin_code)

# m<-gam(log(alldata$mil)~log(alldata$n_spp)+s(log(alldata$TenYear), k=5)+s(log(alldata$sr_orig+1), k=3)+s(log(alldata$sr_dest+1), k=3)+s(log(alldata$trade+1), k=3)+s((alldata$GDP.i+1), k=3)+s((alldata$GDP.j+1), k=3)+s((alldata$Pop.i+1), k=3)+s((alldata$Pop.j+1), k=3)+s(log(alldata$Distance+1), k=3)+ s(log(alldata$totalgivenTenYear), k=3)+s(log(alldata$Reference_ID), k=3), select=T,method='GCV.Cp')
biomes<-read.csv('~/Downloads/shared_biomes.csv')
colnames(biomes)[1:2]<-c("origin_code", "destin_code")
alldata<-merge(alldata, biomes, all.x=T, all.y=F)
alldata$lat.i<-as.numeric(alldata$lat.i)
alldata$lat.j<-as.numeric(alldata$lat.j)
for (i in 24:34)
{
  alldata[which(is.na(alldata[,i])),i]<-mean(alldata[,i], na.rm=T)
  
}

all_IAS<-read.csv('turbelinetal_2017_IAS.csv')
alldata$all_IAS<-all_IAS$SInv....IAS.in.country.[match(alldata$destin_code,all_IAS$ISO3.ISO.2016.)]

cor_preds<-cor(alldata[,c(5:34,37:38)])

#GDP and Cap Form equivalent j is 0.98, i is 0.96, tourism, cap formation and gdp in j all, collinear with # references, tourism and cap formation, gdp and cap formation collinear with tourism, cap form j

#removed for collinearity - gdp i and j (reference ID), tourism
m2<-gam(log(alldata$mil)~log(alldata$n_spp)+s((alldata$TenYear), k=5)+(log(alldata$sr_orig+1))+(log(alldata$sr_dest+1))+(log(alldata$trade+1))+(log(alldata$pop.i+1))+(log(alldata$pop.j+1))+(log(alldata$Distance))+log(alldata$totalgivenTenYear)+log(alldata$area.i)+log(alldata$area.j)+log(alldata$trade_historical+1)+log(alldata$Reference_ID)+log(alldata$Reference_ID_origin)+alldata$CB+alldata$CL+alldata$FTA+alldata$CCH+log(alldata$aff.i)+log(alldata$aff.j)+alldata$lat.i+alldata$lat.j+log(alldata$rd_dens.i+1)+log(alldata$rd_dens.j+1)+log(alldata$food_imports)+log(alldata$cap_form.i+1)+alldata$common_yn)
summary(m2)
cor_preds2<-cor(alldata[,c(5:19,22:24,28:34,37)])

climatezones<-read.csv('~/Downloads/shared_climate_zones.csv')
colnames(climatezones)<-c("origin_code", "destin_code", 'lc2', 'nc2', 'cyn2')
alldata<-merge(alldata, climatezones, all.x=T, all.y=F)

m3<-gam(log(alldata$mil)~log(alldata$n_spp)+s((alldata$TenYear), k=5)+(log(alldata$sr_orig+1))+(log(alldata$sr_dest+1))+(log(alldata$trade+1))+(log(alldata$pop.i+1))+(log(alldata$pop.j+1))+(log(alldata$Distance))+log(alldata$totalgivenTenYear)+log(alldata$area.i)+log(alldata$area.j)+log(alldata$trade_historical+1)+log(alldata$Reference_ID)+log(alldata$Reference_ID_origin)+alldata$CB+alldata$CL+alldata$FTA+alldata$CCH+log(alldata$aff.i)+log(alldata$aff.j)+alldata$lat.i+alldata$lat.j+log(alldata$rd_dens.i+1)+log(alldata$rd_dens.j+1)+log(alldata$food_imports)+log(alldata$cap_form.i+1)+alldata$cyn2)
summary(m3)

gam.check(m2)
abline(0,1)
concurvity(m2)
qq.gam(m2)


sig_trans<-summary(m2)$p.table

untrans<-gam((alldata$mil)~(alldata$n_spp)+s((alldata$TenYear), k=5)+((alldata$sr_orig))+((alldata$sr_dest))+((alldata$trade))+((alldata$pop.i))+((alldata$pop.j))+((alldata$Distance))+(alldata$totalgivenTenYear)+(alldata$area.i)+(alldata$area.j)+(alldata$trade_historical)+(alldata$Reference_ID)+(alldata$Reference_ID_origin)+alldata$CB+alldata$CL+alldata$FTA+alldata$CCH+(alldata$aff.i)+(alldata$aff.j)+alldata$lat.i+alldata$lat.j+(alldata$rd_dens.i)+(alldata$rd_dens.j)+(alldata$food_imports)+(alldata$cap_form.i)+alldata$common_yn)
summary(untrans)
sig_untrans<-summary(untrans)$p.table

gam.check(untrans)
abline(0,1)
qq.gam(untrans)
library(caret)

library(ggplot2)
sig_trans<-as.data.frame(sig_trans)
sig_trans$term<-row.names(sig_trans)
sig_trans$signif<-0
sig_trans$signif[which(sig_trans$`Pr(>|t|)`<0.05)]<-1

library(forcats)
sig_trans<-sig_trans%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_trans, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_trans$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=round(sig_trans$Estimate,2), hjust=-0.2)+scale_x_continuous(limits=c(-12,18))


sig_untrans<-as.data.frame(sig_untrans)
sig_untrans$term<-row.names(sig_untrans)
sig_untrans$signif<-0
sig_untrans$signif[which(sig_untrans$`Pr(>|t|)`<0.05)]<-1
sig_untrans<-sig_untrans%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_untrans, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_untrans$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=formatC(sig_untrans$Estimate, format="e", digits=1), hjust=-0.5, size=3)+scale_x_continuous(limits=c(-10,21))
# m3<-gam((alldata$mil)~(alldata$n_spp)+s((alldata$TenYear), k=5)+s((alldata$sr_orig+1), k=3)+s((alldata$sr_dest+1), k=3)+s((alldata$trade+1), k=3)+s((alldata$GDP.i+1), k=3)+s((alldata$GDP.j+1), k=3)+s((alldata$Pop.i+1), k=3)+s((alldata$Pop.j+1), k=3)+s((alldata$Distance+1), k=3)+s(log(alldata$Reference_ID), k=3), select=T,method='GCV.Cp', family=poisson)
alldata$cost_pub<-alldata$mil/alldata$Reference_ID
m4<-gam(log(alldata$cost_pub)~log(alldata$n_spp)+s((alldata$TenYear), k=5)+(log(alldata$sr_orig+1))+(log(alldata$sr_dest+1))+(log(alldata$trade+1))+(log(alldata$pop.i+1))+(log(alldata$pop.j+1))+(log(alldata$Distance))+log(alldata$totalgivenTenYear)+log(alldata$area.i)+log(alldata$area.j)+log(alldata$trade_historical+1)+log(alldata$Reference_ID)+log(alldata$Reference_ID_origin)+alldata$CB+alldata$CL+alldata$FTA+log(alldata$aff.i)+log(alldata$aff.j)+alldata$lat.i+alldata$lat.j+log(alldata$rd_dens.i+1)+log(alldata$rd_dens.j+1)+log(alldata$food_imports)+log(alldata$cap_form.i+1)+alldata$CCH+alldata$common_yn)
summary(m4)

sig_perpub<-summary(m4)$p.table

sig_perpub<-as.data.frame(sig_perpub)
sig_perpub$term<-row.names(sig_perpub)
sig_perpub$signif<-0
sig_perpub$signif[which(sig_perpub$`Pr(>|t|)`<0.05)]<-1
sig_perpub<-sig_perpub%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_perpub, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_perpub$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=round(sig_perpub$Estimate,2), hjust=-0.1)+scale_x_continuous(limits=c(-16,18))

plot(log(alldata$mil)~predict(m2))

abline(0,1)
saveRDS(m, file="output/countrylevelgam.RDS")
write.csv(alldata, file="data/alldata_pluspreds.csv", row.names=F)
plot(m2, xlab="Decade", ylab="Decadal smoother")

library(mgcViz)
b<-getViz(m2)
plot(pterm(b,13))
plot(pterm(b,12))


countries_give<- expanded %>% group_by(origin_code) %>% summarise(cost=sum(mil))
countries_take<- expanded %>% group_by(destin_code) %>% summarise(cost=sum(mil))

net_give_take<-0
for (i in 1:nrow(countries_give))
{
  net_give_take[i]<--countries_give$cost[i]
  if (any(countries_take$destin_code==countries_give$origin_code[i]))
  {net_give_take[i]=countries_take$cost[which(countries_take$destin_code== countries_give$origin_code[i])]-countries_give$cost[i]}
}
net_give_take<-data.frame(code=countries_give$origin_code,net_give_take=net_give_take)
trade_give_take<-0
for (i in 1:nrow(trade))
{
  trade_give_take[i]<--rowSums(trade)[i]
  if (any(colnames(trade)==rownames(trade)[i]))
  {trade_give_take[i]=colSums(trade)[which(colnames(trade)==rownames(trade)[i])]-rowSums(trade)[i]}
}
trade_give_take<-data.frame(code=rownames(trade),trade_give_take=trade_give_take)

net_trade_spp<-left_join(net_give_take, trade_give_take)
library(fmsb)
net_trade_spp<-net_trade_spp%>%mutate(trade_pc=percent_rank(trade_give_take))
net_trade_spp<-net_trade_spp%>%mutate(spp_pc=percent_rank(net_give_take))
library(viridis)
plot(spp_pc~trade_pc, data=net_trade_spp)
abline(v=0.5, h=0.5)
continent_ref<-subset(expanded, grepl(";",expanded$origin)==F)
net_trade_spp$Continent<-continent_ref$origin[match(net_trade_spp$code, continent_ref$origin_code)]
#write.csv(net_trade_spp, file="missing_continents.csv", row.names=F)
net_trade_spp<-read.csv('missing_continents.csv')


plot(x=c(1:8),y=rep(3,8),col=cbPalette8, pch=19)
ggplot(net_trade_spp,aes(x=trade_pc, y=spp_pc, colour=Continent, trade_pc))+geom_text(label=net_trade_spp$code, key_glyph='rect')+labs(x="Percentile of 21st Century Net Trade", y="Percentile of Net IAS Costs")+theme_classic()+geom_vline(xintercept=0.5)+geom_hline(yintercept=0.5)+scale_colour_manual(values=c("NAm"="#000000", "AF"="#d55e00", "AS"="#56B4E9", "SA"="#009E73", "OC"="#E69F00", "EUR"="#0072B2"), labels=c("North America", "Africa", "Asia", "South America", "Oceania", "Europe"))

# wong.palette <- c("#D55E00","#56B4E9","#E69F00","#000000","#E69F00","#009E73","#d9d9d9", "#0072B2") ## Wong et al. 2011


#Africa, Asia, Oceania, North America, Oceania, South America, Antartica, Europe

boot_10p<-alldata
for (i in 1:(nrow(alldata)%/%10))
{
  boot_10p<-bind_rows(boot_10p, alldata[sample(which(alldata$destin_code!="USA"),1),])
}
m3_10p<-gam(log(boot_10p$mil)~log(boot_10p$n_spp)+s((boot_10p$TenYear), k=5)+(log(boot_10p$sr_orig+1))+(log(boot_10p$sr_dest+1))+(log(boot_10p$trade+1))+(log(boot_10p$pop.i+1))+(log(boot_10p$pop.j+1))+(log(boot_10p$Distance))+log(boot_10p$totalgivenTenYear)+log(boot_10p$area.i)+log(boot_10p$area.j)+log(boot_10p$trade_historical+1)+log(boot_10p$Reference_ID)+log(boot_10p$Reference_ID_origin)+boot_10p$CB+boot_10p$CL+boot_10p$FTA+boot_10p$CCH+log(boot_10p$aff.i)+log(boot_10p$aff.j)+boot_10p$lat.i+boot_10p$lat.j+log(boot_10p$rd_dens.i+1)+log(boot_10p$rd_dens.j+1)+log(boot_10p$food_imports)+log(boot_10p$cap_form.i+1)+boot_10p$common_yn)

sig_10p<-summary(m3_10p)$p.table
sig_10p<-as.data.frame(sig_10p)
sig_10p$term<-row.names(sig_10p)
sig_10p$signif<-0
sig_10p$signif[which(sig_10p$`Pr(>|t|)`<0.05)]<-1
sig_10p<-sig_10p%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_10p, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_10p$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=round(sig_10p$Estimate,2), hjust=-0.1)+scale_x_continuous(limits=c(-16,19))


boot_100p<-alldata
for (i in 1:(nrow(alldata)))
{
  boot_100p<-bind_rows(boot_100p, alldata[sample(which(alldata$destin_code!="USA"),1),])
}
m3_10p<-gam(log(boot_100p$mil)~log(boot_100p$n_spp)+s((boot_100p$TenYear), k=5)+(log(boot_100p$sr_orig+1))+(log(boot_100p$sr_dest+1))+(log(boot_100p$trade+1))+(log(boot_100p$pop.i+1))+(log(boot_100p$pop.j+1))+(log(boot_100p$Distance))+log(boot_100p$totalgivenTenYear)+log(boot_100p$area.i)+log(boot_100p$area.j)+log(boot_100p$trade_historical+1)+log(boot_100p$Reference_ID)+log(boot_100p$Reference_ID_origin)+boot_100p$CB+boot_100p$CL+boot_100p$FTA+boot_100p$CCH+log(boot_100p$aff.i)+log(boot_100p$aff.j)+boot_100p$lat.i+boot_100p$lat.j+log(boot_100p$rd_dens.i+1)+log(boot_100p$rd_dens.j+1)+log(boot_100p$food_imports)+log(boot_100p$cap_form.i+1)+boot_100p$common_yn)

sig_10p<-summary(m3_10p)$p.table
sig_10p<-as.data.frame(sig_10p)
sig_10p$term<-row.names(sig_10p)
sig_10p$signif<-0
sig_10p$signif[which(sig_10p$`Pr(>|t|)`<0.05)]<-1
sig_10p<-sig_10p%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_10p, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_10p$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=round(sig_10p$Estimate,2), hjust=-0.1)+scale_x_continuous(limits=c(-16,25))



boot_50p<-alldata
for (i in 1:(nrow(alldata)))
{
  boot_50p<-bind_rows(boot_50p, alldata[sample(which(alldata$destin_code!="USA"),1),])
}
m3_10p<-gam(log(boot_50p$mil)~log(boot_50p$n_spp)+s((boot_50p$TenYear), k=5)+(log(boot_50p$sr_orig+1))+(log(boot_50p$sr_dest+1))+(log(boot_50p$trade+1))+(log(boot_50p$pop.i+1))+(log(boot_50p$pop.j+1))+(log(boot_50p$Distance))+log(boot_50p$totalgivenTenYear)+log(boot_50p$area.i)+log(boot_50p$area.j)+log(boot_50p$trade_historical+1)+log(boot_50p$Reference_ID)+log(boot_50p$Reference_ID_origin)+boot_50p$CB+boot_50p$CL+boot_50p$FTA+boot_50p$CCH+log(boot_50p$aff.i)+log(boot_50p$aff.j)+boot_50p$lat.i+boot_50p$lat.j+log(boot_50p$rd_dens.i+1)+log(boot_50p$rd_dens.j+1)+log(boot_50p$food_imports)+log(boot_50p$cap_form.i+1)+boot_50p$common_yn)

sig_10p<-summary(m3_10p)$p.table
sig_10p<-as.data.frame(sig_10p)
sig_10p$term<-row.names(sig_10p)
sig_10p$signif<-0
sig_10p$signif[which(sig_10p$`Pr(>|t|)`<0.05)]<-1
sig_10p<-sig_10p%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_10p, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_10p$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=round(sig_10p$Estimate,2), hjust=-0.1)+scale_x_continuous(limits=c(-16,25))

expanded$cap<-0
for (i in 1:nrow(expanded)){
expanded$cap[i]<-alldata$cap_form.i[which(alldata$origin_code==expanded$origin_code[i]& alldata$TenYear==expanded$TenYear[i])][1]
}
for (i in 1:nrow(expanded)){
  if (is.nan(expanded$cap[i]))
  {
    sub<-subset(expanded, origin_code==expanded$origin_code[i]&is.nan(expanded$cap_form.i)==F)
    if (nrow(sub)>0){expanded$cap[i]<-sub$cap[which.min(abs(sub$TenYear-expanded$TenYear[i]))]}
  }}
expanded$cap[which(is.na(expanded$cap))]<-mean(expanded$cap, na.rm=T)
expanded<-expanded %>%
  mutate(mil_wealth = (mil2*cap))%>%group_by(N, destin_code)%>%mutate(mil_wealth=mil_wealth/sum(cap)) 
alldata_wealth<-expanded%>%group_by(destin_code, origin_code, TenYear)%>%summarize_at('mil_wealth', sum)

alldata<-left_join(alldata, alldata_wealth)
# cost qualified by #origins and #destinations using 'N' qualifier
m2_wealth<-gam(log(alldata$mil_wealth+1)~log(alldata$n_spp)+s((alldata$TenYear), k=5)+(log(alldata$sr_orig+1))+(log(alldata$sr_dest+1))+(log(alldata$trade+1))+(log(alldata$pop.i+1))+(log(alldata$pop.j+1))+(log(alldata$Distance))+log(alldata$totalgivenTenYear)+log(alldata$area.i)+log(alldata$area.j)+log(alldata$trade_historical+1)+log(alldata$Reference_ID)+log(alldata$Reference_ID_origin)+alldata$CB+alldata$CL+alldata$FTA+alldata$CCH+log(alldata$aff.i)+log(alldata$aff.j)+alldata$lat.i+alldata$lat.j+log(alldata$rd_dens.i+1)+log(alldata$rd_dens.j+1)+log(alldata$food_imports)+log(alldata$cap_form.i+1)+alldata$common_yn)
summary(m2_wealth)
sig_wealth<-summary(m2_wealth)$p.table
sig_wealth<-as.data.frame(sig_wealth)
sig_wealth$term<-row.names(sig_wealth)
sig_wealth$signif<-0
sig_wealth$signif[which(sig_wealth$`Pr(>|t|)`<0.05)]<-1
sig_wealth<-sig_wealth%>%mutate(term = fct_reorder(term, `t value`))
ggplot(data=sig_wealth, aes(y=term, x=`t value`, fill=signif))+geom_col()+theme_classic()+scale_y_discrete(labels=c("Intercept", " # Species Involved", "Sender Species Richness", "Recipient Species Richness","21st Century Trade", "Sender Human Population", "Recipient Human Population", "Distance", "Total Cost of Sender Country", "Area of Sender Country", "Area of Recipient Country", "20th Century Trade", "Research Effort in Recipient", "Research Effort in Sender", "Common Border", "Common Language", "Free Trade Agreement", "Common Colonial History", "Primary Industries Value Added in Sender", "Primary Industries Value Added in Recipient", "Latitude of Sender", "Latitude of Recipient", "Road Density of Sender", "Road Density of Recipient", "Food Imports", "Gross Capital Formation in Sender", "Shared Biome")[order(sig_wealth$`t value`)])+theme(axis.text.x=element_text(vjust=1, hjust=0.2))+labs(x='Model t-value', y='')+geom_vline(xintercept=0, col='red')+scale_fill_binned(c('lightgrey', 'blue'),guide='none' )+theme(axis.text.y=element_text(size=12, colour='black'))+geom_text(label=round(sig_wealth$Estimate,3), hjust=-0.1)+scale_x_continuous(limits=c(-16,25))
