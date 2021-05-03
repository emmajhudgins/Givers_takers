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


# As well as Emma's prior filters...
# Data adjustments (manual):
# Merged all continental origins into single column
# Merged all continental destinations into single column -- standardised
# column names with origins (CA = NAm; OC-PI = OC; Ant-Sub = A-S)
# Added several blank, non-pathogen origins using Google Sheet entries (Diuraphis noxia = AS;
# Ephestia kuehniella = AS; Rumex lunaria = EUR; Thaumetopoea processionea = EUR)

### PREP by continent

data<-read.csv("invacost_origin_expanded_DN.csv") # continent column manually fixed by Dat Nguyen
data$origin<-NA
for (i in 1:nrow(data))
{
  data$origin[i]<-paste(colnames(data)[32:37][which(data[i,32:37]==1)], collapse=";")
}

data<-data[,c(1:31, 38:267)]
data<-data[,-(which(colnames(data)=="NA."))]

data$origin_code<-"NA"
for (i in 1:nrow(data))
{
  if(length(which(data[i,]==1))>0)
  {
    data$origin_code[i]<-paste(colnames(data)[which(data[i,32:260]==1)+31],collapse=';')
  }
}
data$destin_code<-sapply(data$Official_country,FUN=function(x){paste(countrycode(unlist(strsplit(x, '/')),'country.name', 'iso3c'), collapse=';')})
data <- data[!(data$origin_code==""),] # blank origins (i.e. pathogens) omitted
data <- data[!is.na(data$Cost_estimate_per_year_2017_USD_exchange_rate),] # blank costs removed

data$mil<-data$Cost_estimate_per_year_2017_USD_exchange_rate/1000000
sum(data$mil) # aggregate cost (US$ millions)
data$N <- 1:nrow(data) # unique identifier for qualification below

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

expanded$TenYear<-signif(expanded$Impact_year,digits=3)

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
top_pairs<-aggregate(mil~origin_code*destin_code,data=expanded,FUN=sum)

byspp<-expanded%>%group_by(Species)%>%mutate(num=1/length(N))
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
socioeco_dat<-subset(socioeco_dat, yr==2014)
socioeco_dat<-subset(socioeco_dat, IHS.i%in%countrycode(unique(expanded$destin_code), 'iso3c', 'country.name'))
socioeco_dat<-subset(socioeco_dat, IHS.j%in%countrycode(unique(expanded$destin_code), 'iso3c', 'country.name'))


###trade data

# alldata<-read.csv("invacost_origin_expanded_DN.csv") # continent column manually fixed by Dat Nguyen
# origin_countries<-colnames(alldata)[38:267]
# destin_countries<-countrycode(unique(data$Official_country), 'country.name', 'iso3c')
# # trade<-matrix(0, length(origin_countries),length(destin_countries))
# baci_country<-read.csv('~/Downloads/country_codes_V202102.csv')
# 
# baci_dat<-read.csv(paste0("~/Downloads/BACI_HS92_V202102.csv"))
# for (j in 1:length(destin_countries))
# {
#   for (i in 1:length(origin_countries))
#   {
#     qq<-baci_dat[which(baci_dat$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     if(nrow(qq)>0)
#     {trade[i,j]<-trade[i,j]+sum(qq$q, na.rm=T)}
#   }
# }
# saveRDS(trade, "../output/trade.RDS")
# trade_historical<-matrix(0, length(origin_countries),length(destin_countries))
# 
# baci_dat<-read.csv(paste0("~/Downloads/BACI_HS92_Y1995_V202102.csv"))
# for (j in 1:length(destin_countries))
# {
#   for (i in 1:length(origin_countries))
#   {
#     qq<-baci_dat[which(baci_dat$i==baci_country$country_code[which(baci_country$iso_3digit_alpha%in%origin_countries[i])] & baci_dat$j%in%baci_country$country_code[which(baci_country$iso_3digit_alpha%in%destin_countries[j])]),]
#     if(nrow(qq)>0)
#     {trade_historical[i,j]<-trade_historical[i,j]+sum(qq$q, na.rm=T)}
#   }
# }
#saveRDS(trade_historical, "../output/trade_historical.RDS")

colnames(socioeco_dat)[15:16]<-c('origin_code', 'destin_code')
socioeco_dat$origin_code<-countrycode(socioeco_dat$origin_code,'country.name', 'iso3c')
socioeco_dat$destin_code<-countrycode(socioeco_dat$destin_code,'country.name', 'iso3c')
expanded<-left_join(expanded, socioeco_dat, by=c('origin_code', 'destin_code'))
expanded$FTA[which(is.na(expanded$FTA))]<-"UNK"
expanded$CCH[which(is.na(expanded$CCH))]<-"UNK"
expanded$CB[which(is.na(expanded$CB))]<-"UNK"
expanded$CL[which(is.na(expanded$CL))]<-"UNK"
expanded$GDP.i[which(is.na(expanded$GDP.i))]<-mean(expanded$GDP.i, na.rm=T)
expanded$GDP.j[which(is.na(expanded$GDP.j))]<-mean(expanded$GDP.j, na.rm=T)
expanded$Pop.i[which(is.na(expanded$Pop.i))]<-mean(expanded$Pop.i, na.rm=T)
expanded$Pop.j[which(is.na(expanded$Pop.j))]<-mean(expanded$Pop.j, na.rm=T)
expanded$Distance[which(is.na(expanded$Distance))]<-mean(expanded$Distance, na.rm=T)

origin_countries<-colnames(data)[38:267]
destin_countries<-countrycode(unique(data$Official_country), 'country.name', 'iso3c')
trade<-readRDS('../output/trade.RDS')
trade_historical<-readRDS('../output/trade_historical.RDS')

### check out top trade associations
top10pairs<-arrayInd(order(trade, decreasing=T)[1:11], dim(trade))

top10pairs[,1]<-origin_countries[top10pairs[,1]]
top10pairs[,2]<-destin_countries[as.numeric(top10pairs[,2])]
colnames(top10pairs)<-c("From", 'To')
top10pairs<-as.data.frame(top10pairs)
top10pairs$Vol<-unlist(trade[order(trade, decreasing=T)[1:11]])
top10pairs<-top10pairs[-c(3),] # removing duplicate Canada, Venezuela, Mexico ->USA (due to duplicate USA codes)


historical_pairs<-arrayInd(order(trade_historical, decreasing=T)[1:13], dim(trade_historical))

historical_pairs[,1]<-origin_countries[historical_pairs[,1]]
historical_pairs[,2]<-destin_countries[as.numeric(historical_pairs[,2])]
colnames(historical_pairs)<-c("From", 'To')
historical_pairs<-as.data.frame(historical_pairs)
historical_pairs$Vol<-unlist(trade_historical[order(trade_historical, decreasing=T)[1:13]])
historical_pairs<-historical_pairs[-c(2,5,7),]# removing duplicate Canada ->USA


### Model cost donation/reception
expanded<-cbind(expanded, byspp$num)
alldata<-expanded%>%group_by(origin_code, destin_code, TenYear)%>%summarise_at(c('mil','...280' ),sum)

alldata2<-cbind(expanded)%>%group_by(origin_code, destin_code, TenYear)%>%summarise_at(c('GDP.i', 'GDP.j', 'Pop.i', 'Pop.j', 'Distance'),mean)

alldata3<-cbind(expanded)%>%group_by(origin_code, destin_code, TenYear)%>%summarise_at(c('CB', 'CL', 'CCH', 'FTA'),max)
alldata<-cbind(alldata, alldata2[,4:8], alldata3[,4:7])
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
for (i in 2:7)
{
  sr_tab[,i]<-gsub(",", "",sr_tab[,i])
  
  sr_tab[,i]<-as.numeric(sr_tab[,i])
}
sr_tab$code<-countrycode(sr_tab$Country, 'country.name', 'iso3c')
sr_tab$tot_sr<-rowSums(sr_tab[,2:7], na.rm=T)

alldata$sr_orig<-sr_tab$tot_sr[match(alldata$origin_code,sr_tab$code)]
alldata$sr_dest<-sr_tab$tot_sr[match(alldata$destin_code,sr_tab$code)]
alldata$sr_orig[which(is.na(alldata$sr_orig))]<-mean(alldata$sr_orig, na.rm=T)
alldata$sr_dest[which(is.na(alldata$sr_dest))]<-mean(alldata$sr_dest, na.rm=T)


m<-gam(log(alldata$mil)~log(alldata$n_spp)+s(log(alldata$TenYear), k=5)+s(log(alldata$sr_orig+1), k=3)+s(log(alldata$sr_dest+1), k=3)+s(log(alldata$trade+1), k=3)+s(log(alldata$GDP.i+1), k=3)+s(log(alldata$GDP.j+1), k=3)+s(log(alldata$Pop.i+1), k=3)+s(log(alldata$Pop.j+1), k=3)+s(log(alldata$Distance+1), k=3), select=T,method='GCV.Cp')


m2<-lm(log(alldata$mil)~log(alldata$n_spp)+(log(alldata$TenYear))+(log(alldata$sr_orig+1))+(log(alldata$sr_dest+1))+(log(alldata$trade+1))+(log(alldata$GDP.i+1))+(log(alldata$GDP.j+1))+(log(alldata$Pop.i+1))+(log(alldata$Pop.j+1))+(log(alldata$Distance+1)))

m3<-gam((alldata$mil)~(alldata$n_spp)+s((alldata$TenYear), k=5)+s((alldata$sr_orig+1), k=3)+s((alldata$sr_dest+1), k=3)+s((alldata$trade+1), k=3)+s((alldata$GDP.i+1), k=3)+s((alldata$GDP.j+1), k=3)+s((alldata$Pop.i+1), k=3)+s((alldata$Pop.j+1), k=3)+s((alldata$Distance+1), k=3), select=T,method='GCV.Cp', family=poisson)


summary(m)


plot(log(alldata$mil)~predict(m))

abline(0,1)
saveRDS(m, file="../output/countrylevelgam.RDS")
write.csv(alldata, file="alldata_pluspreds.csv", row.names=F)

