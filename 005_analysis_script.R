rm(list=ls())
require(dplyr)
require(tidyr)
require(ggplot2)
require(mgsub)
require(countrycode)
require(gridExtra)
options(scipen=999)

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

data<-data[,c(1:32, 283,285)]
colnames(data)[33:34]<-c("Destin_cont", "Origin_cont")
data$Origin_cont[which(data$Species=='Ephestia kuehniella')]<-"AS"
data$Origin_cont[which(data$Species=='Rumex lunaria')]<-"EUR"
data$Origin_cont[which(data$Species=='Thaumetopoea processionea')]<-"EUR"

data_unk_cont<-data[(data$Origin_cont==""),]
data <- data[!(data$Origin_cont==""),] # blank origins (i.e. pathogens) omitted
data <- data[!is.na(data$Cost_estimate_per_year_2017_USD_exchange_rate),] # blank costs removed
data$mil<-data$Cost_estimate_per_year_2017_USD_exchange_rate/1000000
sum(data$mil) # aggregate cost (US$ millions)
data$N <- 1:nrow(data) # unique identifier for qualification below
data$TenYear<-signif(data$Impact_year,digits=3)
data<-subset(data, (TenYear>=1960&TenYear<2020))
length(unique(data$Cost_ID))
length(unique(data$Species))
length(unique(data$Reference_ID))
domesticated<-c('Felis catus', 'Canis lupus', 'Ovis aries', 'Camelus dromedarius','Sus scrofa','Equus caballus','Equus asinus', 'Mustela furo','Capra hircus', "Bos taurus")
domesticated[which(domesticated%in%data$Species==F)]
data<-subset(data, Species%in%domesticated==F)
length(unique(data$Species))
length(unique(data$Cost_ID))
length(unique(data$Reference_ID))
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
unique(expanded$Origin_cont)
unique(expanded$Destin_cont)

### BASIC ANALYSES

receive<-aggregate(mil~Destin_cont,data=expanded,FUN=sum)
saveRDS(receive, file="output/received.RDS")
given<-aggregate(mil~Origin_cont,data=expanded,FUN=sum)
saveRDS(given, file="output/given.RDS")
flows<-aggregate(mil~Origin_cont*Destin_cont,data=expanded,FUN=sum)

flow2<-aggregate(mil~Origin_cont*Destin_cont+Species,data=expanded,FUN=sum)
flow2<-flow2 %>%
  mutate(N = 1) # qualifying factor
flow3<-flow2 %>%
  group_by(Species) %>%
  mutate(N = N / n()) # number of species qualified per origin/destination
byspp<-aggregate(N~Origin_cont*Destin_cont,data=flow3,FUN=sum)
bycost<-aggregate(mil~Origin_cont*Destin_cont,data=flow3,FUN=sum)
colnames(byspp)[3]<-"num"
saveRDS(byspp, file="output/flows_species.RDS")
saveRDS(bycost, file="output/flows_cost.RDS")
receive_spp<-aggregate(num~Destin_cont,data=byspp,FUN=sum)
saveRDS(receive_spp, "output/receive_spp.RDS")
given_spp<-aggregate(num~Origin_cont,data=byspp,FUN=sum)
saveRDS(given_spp, "output/given_spp.RDS")
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
pdf(file='output/invacost_givers.pdf')
par(oma=c(0,0,0,4))
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()
saveRDS(db, file="output/invacost_givers_data.RDS")
plots<-list()
i=1
for (reg in region_cd)
{
  
  plots[[i]]<-ggplot(subset(db2, Destin_cont==reg), aes(x=TenYear,y=cost))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+
    theme_bw()+theme_classic()+labs(title=region_st[i])+xlab("Time")+ylab("Cost")
  i=i+1
}
pdf(file='output/invacost_receivers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()
saveRDS(db2, file="output/invacost_takers_data.RDS")
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
pdf(file='output/invacost_spp_givers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()
saveRDS(db3, file="output/invacost_givers_spp.RDS")
plots<-list()
i=1
for (reg in region_cd)
{
  
  plots[[i]]<-ggplot(subset(db4, Destin_cont==reg), aes(x=TenYear,y=spp))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+
    theme_bw()+theme_classic()+labs(title=region_st[i])+xlab("Time")+ylab("No. species")
  i=i+1
}
pdf(file='output/invacost_spp_receivers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()

saveRDS(db4, file="output/invacost_takers_spp.RDS")
stwist<-read.table('data/sTwist_database.csv', header=T)

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
tempo <- seq(1850, 2010, by=10)

for (reg in region){
  
  d_sub <- dplyr::filter(st, Destin_cont == reg)
  for (j in 1:length(tempo)){
    tp <- tempo[j]
    if(isTRUE(tp %in% d_sub$TenYear))
      next
    st <- add_row(st, Destin_cont = reg, TenYear = tp, N = 0)
  }
}
st<-subset(st, TenYear>=1850)
plots<-list()
i=1
for (reg in region)
{
  plots[[i]]<-ggplot(subset(st, Destin_cont==reg), aes(x=TenYear,y=N))+
    geom_histogram(stat="identity",color="black", fill="lightblue")+theme_bw()+theme_classic()+scale_y_continuous(limits=range(st$N))+labs(title=reg)+xlab("Time")+ylab("Species")
  i=i+1
}

pdf(file='output/sTwist_receivers.pdf')
grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=2)
dev.off()


full_data<-read.csv('data/InvaCost_database_v4.1.csv')
full_data<-subset(full_data,Method_reliability=="High" )
full_data<-subset(full_data,Implementation=="Observed" )
full_data<-subset(full_data, Probable_starting_year_adjusted>=1960)
full_data<-subset(full_data, Probable_ending_year_adjusted<=2019)
full_data<-subset(full_data, Species%in%domesticated==F)
viruses<-read.csv('givers_takers_doublecheck_fourpointone.csv')
viruses<-unique(viruses$Species[which(viruses$Disease.Agent..to.remove.==1)])
full_data<-subset(full_data, Species%in%viruses==F)
full_data<-subset(full_data, grepl("Unit",full_data$Spatial_scale)==F)
invacost_sub<-subset(full_data, Species=="Diverse/Unspecified")
length(grep("spp\\.", full_data$Species))
invacost_sub<-bind_rows(invacost_sub, full_data[grep("spp\\.", full_data$Species),])
data<-expanded[,2:4]# continent column manually fixed by Dat Nguyen
invacost_per_reg<-full_data%>%group_by(Geographic_region)%>%summarize_at('Cost_estimate_per_year_2017_USD_exchange_rate', sum, na.rm=T)
data<-unique.data.frame(data)
invacost_sub2<-subset(full_data, Species%in%data$Species==F)
invacost_sub2<-invacost_sub2[,1:66]
length(unique(invacost_sub2$Species))
invacost_sub<-bind_rows(invacost_sub, invacost_sub2)
invacost_sub<-invacost_sub[,c(2,4,19,29,47)]
invacost_sub<-unique(data.frame(invacost_sub))
length(unique(invacost_sub$Species))

sample<-invacost_sub%>%group_by(Geographic_region)%>%summarize_at('Cost_ID', n_distinct)
sample2<-invacost_sub%>%group_by(Geographic_region)%>%summarize_at('Cost_estimate_per_year_2017_USD_exchange_rate', sum, na.rm=T)
sample2<-merge(sample2,invacost_per_reg, by="Geographic_region")
sample2<-sample2%>%mutate(prop=Cost_estimate_per_year_2017_USD_exchange_rate.x/Cost_estimate_per_year_2017_USD_exchange_rate.y)
library(ggplot2)
sample2$label<-paste0(sample2$Geographic_region, ", $", round(sample2$Cost_estimate_per_year_2017_USD_exchange_rate.x/1000000, digits=2), ", (", sample$Cost_ID, ")")
pdf('/output/Missing_costs.pdf')
ggplot(data=sample2, aes(y=prop, x=reorder(label,prop)))+
  geom_bar(stat="identity")+theme_classic()+
  ylab(label="Proportion of missing costs")+
  xlab(label=NULL)+
  coord_flip()+
  theme(axis.ticks=element_blank(), axis.text.x=element_text(size=10),legend.position = "none")
dev.off()
       
       