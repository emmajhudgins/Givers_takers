rm(list=ls())
require(dplyr)
require(tidyr)
require(here)
require(ggplot2)
require(mgsub)
require(countrycode)
require(gridExtra)
setwd(paste0(here(), "/data/"))
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

data<-read.csv("invacost_origin_expanded_fourpointone_DN2.csv") # continent column manually fixed by Dat Nguyen

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



length(unique(expanded$Species))
length(unique(expanded$Cost_ID))
length(unique(expanded$Reference_ID))
length(unique(expanded$N))
length(unique(expanded$origin_code))
length(unique(expanded$destin_code))

### BASIC ANALYSES

receivers<-aggregate(mil~destin_code,data=expanded,FUN=sum)
givers<-aggregate(mil~origin_code,data=expanded,FUN=sum)
top_pairs<-aggregate(mil~origin_code*destin_code,data=expanded,FUN=sum, na.rm=T)

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


##link with sTwist first record database

stwist<-read.table('sTwist_database.csv', header=T)

colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$eventDate<-gsub(";.*","",stwist$eventDate )
stwist$eventDate<-as.numeric(stwist$eventDate) # use minimum first record date

#Assign iso3c codes#
stwist$code<-countrycode(stwist$Official_country, 'country.name', 'iso3c')
continent<-read.csv('country-origin-ref_DN.csv')
continent$code<-countrycode(continent$Official_country, 'country.name', 'iso3c')
stwist$Destin_cont<-continent$Geographic_region2[match(stwist$code,continent$code)]

stwist$TenYear<-signif(stwist$eventDate, digits=3)
stwist<-subset(stwist, eventDate>1500)#use post-colonial invasions
domesticated<-c('Felis catus', 'Canis lupus', 'Ovis aries', 'Camelus dromedarius','Sus scrofa','Equus caballus','Equus asinus', 'Mustela furo','Capra hircus')
for (dom in domesticated)
{
  stwist<-stwist[-grep(dom, stwist$scientificName)]
}
stwist_cont<-aggregate(locationID~Destin_cont+Species+TenYear,data=stwist,FUN=length)
stwist_cont<-subset(stwist_cont, Species%in%expanded$Species)
stwist_cont<-stwist_cont %>%
  mutate(N = 1) # qualifying factor 
stwist_cont<-stwist_cont %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination

socioeco_dat<-readRDS('soc_econ_country.rds') # From Sardain, Leung et al. Nature Sustainability
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
trade_historical<-readRDS('../../biosecurity_invacost/data/trade_historical_averaged.RDS')
trade<-readRDS('../../biosecurity_invacost/data/trade_averaged.RDS')
trade<-trade[-163,]
colnames(socioeco_dat)[15:16]<-c('origin_code', 'destin_code')
socioeco_dat$origin_code<-countrycode(socioeco_dat$origin_code,'country.name', 'iso3c')
socioeco_dat$destin_code<-countrycode(socioeco_dat$destin_code,'country.name', 'iso3c')
socioeco_dat<-socioeco_dat%>%group_by(origin_code, destin_code)%>%summarize_at(c( 'FTA', "CCH", "CL", "CB"), max)
data(dist_cepii)
colnames(dist_cepii)[1:2]<-c('origin_code', 'destin_code')
socioeco_dat<-merge(socioeco_dat,dist_cepii, by=c('origin_code', 'destin_code'))
socioeco_dat<-socioeco_dat[,c(1:3,7,9,11,15)]
colnames(socioeco_dat)[4:7]<-c("CB", "CL", "CCH", "Distance")
expanded<-left_join(expanded, socioeco_dat, by=c('origin_code', 'destin_code'))
colnames(socioeco_dat)[1:2]<-colnames(socioeco_dat)[2:1]

for (i in which(is.na(expanded$Distance)))
{
  if (length(which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i])))>0){
  expanded$Distance[i]<-dist_cepii$dist[which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i]))]
  }
}
for (i in which(is.na(expanded$CL)))
{
  if (length(which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i])))>0){
    expanded$CL[i]<-dist_cepii$comlang_ethno[which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i]))]
  }
}
for (i in which(is.na(expanded$CCH)))
{
  if (length(which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i])))>0){
    expanded$CCH[i]<-dist_cepii$comcol[which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i]))]
  }
}
for (i in which(is.na(expanded$CB)))
{
  if (length(which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i])))>0){
    expanded$CB[i]<-dist_cepii$contig[which(dist_cepii$origin_code==expanded$destin_code[i]& which(dist_cepii$destin_code==expanded$origin_code[i]))]
  }
}
gravity<-readRDS('Gravity_V202102.Rds')
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



historical_pairs<-arrayInd(order(trade_historical, decreasing=T)[1:10], dim(trade_historical))

historical_pairs[,1]<-rownames(trade)[historical_pairs[,1]]
historical_pairs[,2]<-colnames(trade)[as.numeric(historical_pairs[,2])]
colnames(historical_pairs)<-c("From", 'To')
historical_pairs<-as.data.frame(historical_pairs)
historical_pairs$Vol<-unlist(trade_historical[order(trade_historical, decreasing=T)[1:10]])



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
area<-wb_data("AG.LND.TOTL.K2", country=unique(alldata$destin_code), start_date = 2018, end_date=2018)
area<-area[,c('iso3c','AG.LND.TOTL.K2')]
colnames(area)[1]<-'destin_code'
alldata<-merge(alldata, area, by='destin_code')
colnames(alldata)[16]<-'area.j'
area<-wb_data("AG.LND.TOTL.K2", country=unique(alldata$origin_code), start_date = 2018, end_date=2018)
area<-area[,c('iso3c','AG.LND.TOTL.K2')]
colnames(area)[1]<-'origin_code'
alldata<-merge(alldata, area, by='origin_code')
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
length(which(is.nan(alldata$pop.i)))#1
length(which(is.nan(alldata$pop.j)))#0
length(which(is.nan(alldata$gdp.i)))#372
length(which(is.nan(alldata$gdp.j)))#0

for (i in 1:nrow(alldata))
{
  if (is.nan(alldata$pop.i[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$pop.i)==F)
    alldata$pop.i[i]<-sub$pop.i[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
  if (is.nan(alldata$pop.j[i]))
  {
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$pop.j)==F)
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
    sub<-subset(alldata, origin_code==alldata$origin_code[i]& is.nan(alldata$gdp.j)==F)
    alldata$gdp.j[i]<-sub$gdp.j[which.min(abs(sub$TenYear-alldata$TenYear[i]))]
  }
}

alldata<-subset(alldata, origin_code%in%c("GIB", "VGB")==F)# removing Gibraltar
alldata$gdp.i[which(alldata$origin_code=="PRK")]<-mean(alldata$gdp.i, na.rm=T)
length(unique(c(alldata$origin_code,alldata$destin_code)))
alldata$origin_code<-as.factor(alldata$origin_code)
alldata$destin_code<-as.factor(alldata$destin_code)
levels(alldata$destin_code)<-levels(alldata$origin_code)

# m<-gam(log(alldata$mil)~log(alldata$n_spp)+s(log(alldata$TenYear), k=5)+s(log(alldata$sr_orig+1), k=3)+s(log(alldata$sr_dest+1), k=3)+s(log(alldata$trade+1), k=3)+s((alldata$GDP.i+1), k=3)+s((alldata$GDP.j+1), k=3)+s((alldata$Pop.i+1), k=3)+s((alldata$Pop.j+1), k=3)+s(log(alldata$Distance+1), k=3)+ s(log(alldata$totalgivenTenYear), k=3)+s(log(alldata$Reference_ID), k=3), select=T,method='GCV.Cp')

m2<-gam(log(alldata$mil)~log(alldata$n_spp)+s((alldata$TenYear), k=5)+(log(alldata$sr_orig+1))+(log(alldata$sr_dest+1))+(log(alldata$trade+1))+(log(alldata$gdp.i+1))+(log(alldata$gdp.j+1))+(log(alldata$pop.i+1))+(log(alldata$pop.j+1))+(log(alldata$Distance))+log(alldata$totalgivenTenYear)+log(alldata$area.i)+log(alldata$area.j)+log(alldata$trade_historical+1)+log(alldata$Reference_ID)+log(alldata$Reference_ID_origin)+alldata$CB+alldata$CL+alldata$FTA+alldata$CCH)
cor(alldata[,5:22])
cor(alldata[,5:22])[order(cor(alldata[,5:22]), decreasing = T)][19:30]
# m3<-gam((alldata$mil)~(alldata$n_spp)+s((alldata$TenYear), k=5)+s((alldata$sr_orig+1), k=3)+s((alldata$sr_dest+1), k=3)+s((alldata$trade+1), k=3)+s((alldata$GDP.i+1), k=3)+s((alldata$GDP.j+1), k=3)+s((alldata$Pop.i+1), k=3)+s((alldata$Pop.j+1), k=3)+s((alldata$Distance+1), k=3)+s(log(alldata$Reference_ID), k=3), select=T,method='GCV.Cp', family=poisson)


summary(m2)


plot(log(alldata$mil)~predict(m2))

abline(0,1)
saveRDS(m, file="../output/countrylevelgam.RDS")
write.csv(alldata, file="alldata_pluspreds.csv", row.names=F)
plot(m2, xlab="Decade", ylab="Decadal smoother")
