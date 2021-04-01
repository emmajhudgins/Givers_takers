rm(list=ls())
require(dplyr)
require(tidyr)
require(here)
require(ggplot2)
require(mgsub)
require(countrycode)
setwd(paste0(here(), "/data/"))
options(scipen=999)

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

data<-data[,c(1:31, 269:270)]
colnames(data)[32:33]<-c("Destin_cont", "Origin_cont")
data$Origin_cont[which(data$Species=='Ephestia kuehniella')]<-"AS"
data$Origin_cont[which(data$Species=='Rumex lunaria')]<-"EUR"
data$Origin_cont[which(data$Species=='Thaumetopoea processionea')]<-"EUR"
data <- data[!(data$Origin_cont==""),] # blank origins (i.e. pathogens) omitted
data <- data[!is.na(data$Cost_estimate_per_year_2017_USD_exchange_rate),] # blank costs removed

data$mil<-data$Cost_estimate_per_year_2017_USD_exchange_rate/1000000
sum(data$mil) # aggregate cost (US$ millions)
data$N <- 1:nrow(data) # unique identifier for qualification below

# Dividing costs among multiple origins

expanded<-data %>% 
  mutate(Origin_cont = strsplit(as.character(Origin_cont), ";")) %>% 
  unnest(Origin_cont) # entries expanded per origin

expanded<-expanded %>% 
  mutate(Destin_cont = strsplit(as.character(Destin_cont), ";")) %>% 
  unnest(Destin_cont) # entries expanded again per destination

expanded<-expanded %>%
  group_by(N) %>%
  mutate(mil = mil / n()) # cost qualified by #origins and #destinations using 'N' qualifier

sum(expanded$mil) # same cost as before origin/destination expansion (it worked)
sum(data$mil) 
expanded$TenYear<-signif(expanded$Impact_year,digits=3)


### BASIC ANALYSES

aggregate(mil~Destin_cont,data=expanded,FUN=sum)
aggregate(mil~Origin_cont,data=expanded,FUN=sum)
aggregate(mil~Origin_cont*Destin_cont,data=expanded,FUN=sum)

region <- unique(expanded$Origin_cont)
tempo <- unique(expanded$TenYear)


df<-aggregate(mil~Origin_cont*Destin_cont+Species+TenYear,data=expanded,FUN=sum)
df<-df %>%
  mutate(N = 1) # qualifying factor 
df<-df %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination

Give <- expanded %>% group_by(Origin_cont, TenYear) %>% summarise(cost=sum(mil))

db <- data.frame(Give[,c("Origin_cont", "TenYear", "cost")])

for (i in 1:length(region)){
  
  or_ct <- region[i]
  d_sub <- dplyr::filter(db, Origin_cont == or_ct)
  
  for (j in 1:length(tempo)){
    tp <- tempo[j]
    if(isTRUE(tp %in% d_sub$TenYear))
      next
    db <- add_row(db, Origin_cont = or_ct, TenYear = tp, cost = 0)
  }
}

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
stwist_cont<-aggregate(locationID~Destin_cont+Species+TenYear,data=stwist,FUN=length)

stwist<-stwist %>%
  mutate(N = 1) # qualifying factor 
stwist<-stwist %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination
domesticated<-c('Felis catus', 'Canis lupus', 'Ovis aries', 'Camelus dromedarius','Sus scrofa','Equus caballus','Equus asinus', 'Mustela furo','Capra hircus')
for (dom in domesticated)
{
stwist<-stwist[-grepl(dom, stwist$Species)]
}
### merge databases##
allcountry_spp<-merge(invacost_cln, stwist, by=c("Species", "code"), all.x=T)
allcountry_spp<-unique(allcountry_spp)
allcountry_spp<-allcountry_spp[,c(1:17,22)]# remove unnecessary columns
colnames(allcountry_spp)[c(11,17)]<-c("Official_country","Cost.USD")

for (reg in region)
{
cont_give<-ggplot(subset(db, Origin_cont==reg), aes(x=TenYear,y=cost))+
  geom_histogram(stat="identity",color="black", fill="lightblue")+
  theme_bw()+theme_classic()
cont_give
}
