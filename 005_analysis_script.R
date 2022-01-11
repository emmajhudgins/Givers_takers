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

data<-data[,c(1:31, 283:284)]
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

byspp<-expanded%>%group_by(Species)%>%mutate(num=1/length(N))
aggregate(num~Destin_cont,data=byspp,FUN=sum)
aggregate(num~Origin_cont,data=byspp,FUN=sum)
aggregate(num~Origin_cont*Destin_cont,data=byspp,FUN=sum)



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


Take <- expanded %>% group_by(Destin_cont, TenYear) %>% summarise(cost=sum(mil))
db2 <- data.frame(Take[,c("Destin_cont", "TenYear", "cost")])

for (i in 1:length(region)){
  
  or_ct <- region[i]
  d_sub <- dplyr::filter(db2, Destin_cont == or_ct)
  
  for (j in 1:length(tempo)){
    tp <- tempo[j]
    if(isTRUE(tp %in% d_sub$TenYear))
      next
    db2 <- add_row(db2, Destin_cont = or_ct, TenYear = tp, cost = 0)
  }
}

region_st<-c("Africa",        "Asia"  ,        "Europe"  ,      "North America",
"Oceania" ,      "South America") #taken from stwist
region_cd<-c('AF','AS','EUR', "NAm", "OC", "SA")#corresponding codes
plots<-list()
i=1
for (reg in region_cd)
{
  
  plots[[i]]<-ggplot(subset(db, Origin_cont==reg), aes(x=TenYear,y=cost))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+
    theme_bw()+theme_classic()+labs(title=region_st[i])+xlab("Time")+ylab("Cost")
  i=i+1
}
pdf(file='../output/invacost_givers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()

plots<-list()
i=1
for (reg in region_cd)
{
  
  plots[[i]]<-ggplot(subset(db2, Destin_cont==reg), aes(x=TenYear,y=cost))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+
    theme_bw()+theme_classic()+labs(title=region_st[i])+xlab("Time")+ylab("Cost")
  i=i+1
}
pdf(file='../output/invacost_receivers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()

Give_spp <- df %>% group_by(Origin_cont, TenYear) %>% summarise(spp=sum(N))

db3 <- data.frame(Give_spp[,c("Origin_cont", "TenYear", "spp")])

for (i in 1:length(region)){
  
  or_ct <- region[i]
  d_sub <- dplyr::filter(db3, Origin_cont == or_ct)
  
  for (j in 1:length(tempo)){
    tp <- tempo[j]
    if(isTRUE(tp %in% d_sub$TenYear))
      next
    db3 <- add_row(db3, Origin_cont = or_ct, TenYear = tp, spp = 0)
  }
}


Take_spp <- df %>% group_by(Destin_cont, TenYear) %>% summarise(spp=sum(N))

db4 <- data.frame(Take_spp[,c("Destin_cont", "TenYear", "spp")])

for (i in 1:length(region)){
  
  or_ct <- region[i]
  d_sub <- dplyr::filter(db4, Destin_cont == or_ct)
  
  for (j in 1:length(tempo)){
    tp <- tempo[j]
    if(isTRUE(tp %in% d_sub$TenYear))
      next
    db4 <- add_row(db4, Destin_cont = or_ct, TenYear = tp, spp = 0)
  }
}

plots<-list()
i=1
for (reg in region_cd)
{
  
  plots[[i]]<-ggplot(subset(db3, Origin_cont==reg), aes(x=TenYear,y=spp))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+
    theme_bw()+theme_classic()+labs(title=region_st[i])+xlab("Time")+ylab("No. Species")
  i=i+1
}
pdf(file='../output/invacost_spp_givers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()

plots<-list()
i=1
for (reg in region_cd)
{
  
  plots[[i]]<-ggplot(subset(db4, Destin_cont==reg), aes(x=TenYear,y=spp))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+
    theme_bw()+theme_classic()+labs(title=region_st[i])+xlab("Time")+ylab("No. species")
  i=i+1
}
pdf(file='invacost_spp_receivers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()

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
  stwist<-stwist[-grep(dom, stwist$scientificName),]
}
stwist_cont<-aggregate(locationID~Destin_cont+Species+TenYear,data=stwist,FUN=length)
stwist_cont<-subset(stwist_cont, Species%in%expanded$Species)
stwist_cont<-stwist_cont %>%
  mutate(N = 1) # qualifying factor 
stwist_cont<-stwist_cont %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination

st <-data.frame((stwist_cont[,c("Destin_cont", "TenYear", "N")])%>%group_by(Destin_cont, TenYear)%>%summarise_at('N', sum))

region <- unique(st$Destin_cont)
tempo <- unique(expanded$TenYear)

for (reg in region){
  
  d_sub <- dplyr::filter(st, Destin_cont == reg)
  for (j in 1:length(tempo)){
    tp <- tempo[j]
    if(isTRUE(tp %in% d_sub$TenYear))
      next
    st <- add_row(st, Destin_cont = reg, TenYear = tp, N = 0)
  }
}
plots<-list()
i=1
for (reg in region)
{
  plots[[i]]<-ggplot(subset(st, Destin_cont==reg), aes(x=TenYear,y=N))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+theme_bw()+theme_classic()+scale_y_continuous(limits=range(st$N))+labs(title=reg)+xlab("Time")+ylab("Species")
  i=i+1
}

pdf(file='../output/sTwist_receivers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()

lpi<-readRDS('~/Downloads/test_continent_red.rds') # need to change how central america is coded here
lpi$Origin_cont<-c("AF", "SA", "NAm", "AS", "EUR", "OC")
lpi$frac_1[which(is.na(lpi$frac_1))]<-0
lpi$frac_2[which(is.na(lpi$frac_2))]<-0
lpi$frac_3[which(is.na(lpi$frac_3))]<-0



n_refs<-expanded%>%group_by(TenYear, Origin_cont)%>%summarize_at("Reference_ID", n_distinct)
Give<-merge(Give, n_refs)
Give<-merge(Give, lpi, "Origin_cont")
library(mgcv)
m<-gam(log(Give$cost)~log(Give_spp$spp)+s(log(Give_spp$TenYear), k=5)+Give_spp$Origin_cont+Give$frac_1+Give$frac_2+Give$frac_3+Give$comb_mean+log(Give$Reference_ID), select=T,method='GCV.Cp')

lpi$Destin_cont<-c("AF", "SA", "NAm", "AS", "EUR", "OC")

Take_spp<-merge(Take_spp, lpi, "Destin_cont")
Take_spp<-merge(Take_spp, n_refs)
m2<-gam(log(Take$cost)~log(Take_spp$spp)+Take_spp$Destin_cont+s(log(Take_spp$TenYear), k=5)+Take$frac_1+Take$frac_2+Take$frac_3+Take$comb_mean+log(Take$Reference_ID),select=T, method='GCV.Cp')

Take_cost<-log(aggregate(mil~Destin_cont,data=expanded,FUN=sum)$mil)
Give_cost<-log(aggregate(mil~Origin_cont,data=expanded,FUN=sum)$mil)

cor.test(log(Take_cost),log(Give_cost)) #only 6 data points

plot(log(Give$cost)~predict(m))
abline(0,1)
plot(log(Take$cost)~predict(m2))
abline(0,1)

