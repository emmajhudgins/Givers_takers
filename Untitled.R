## management cost - biodiversity model
library(countrycode)
library(dplyr)
library(rvest)
library(reshape2)
dam<-read.csv('~/Downloads/damag.ex.or.csv')
mg<-read.csv('~/Downloads/manag.ex.or.csv')
sum<-read.csv('~/Downloads/national.sums.csv')
sum$country<-gsub('Salvador', "El Salvador", sum$country)
dam$code<-countrycode(dam$Official_country,'country.name', 'iso3c')
mg$code<-countrycode(mg$Official_country,'country.name', 'iso3c')
sum$code<-countrycode(sum$country,'country.name', 'iso3c')


stwist<-read.table('~/Desktop/OneDrive - McGill University/GitHub/Givers_takers/data/sTwist_database.csv', header=T)

colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$eventDate<-gsub(";.*","",stwist$eventDate )
stwist$eventDate<-as.numeric(stwist$eventDate) # use minimum first record date

#Assign iso3c codes#
stwist$code<-countrycode(stwist$Official_country, 'country.name', 'iso3c')
continent<-read.csv('~/Desktop/OneDrive - McGill University/GitHub/Givers_takers/data/country-origin-ref_DN.csv')
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

dam$TenYear<-signif(dam$Impact_year,digits=3)
mg$TenYear<-signif(mg$Impact_year,digits=3)
dam_sum<-dam%>%group_by(Species, TenYear, code)%>%summarise_at("cost.bil", list(mean,length)) # not sure why a sum doesnt work in this line but i back out a sum from mean and length
dam_sum$cost.bil<-dam_sum$fn1*dam_sum$fn2
dam_sum<-subset(dam_sum, is.na(dam_sum$code)==F)

mg_sum<-mg%>%group_by(Species, TenYear, code, Management_type)%>%summarise_at("cost.bil", list(mean,length)) # not sure why a sum doesnt work in this line but i back out a sum from mean and length
mg_sum$cost.bil<-mg_sum$fn1*mg_sum$fn2
mg_sum<-subset(mg_sum, is.na(mg_sum$code)==F)
mg_sum<-mg_sum[,c(1:4,7)]
mg_sum2<-dcast(mg_sum, cost.bil+Species+code+TenYear~Management_type,mean, na.rm=T)
combined<-merge(dam_sum, mg_sum2, by=c("Species", "code", "TenYear"))
combined<-combined[,c(1:3, 6,8:11)]
colnames(combined)[4]<-c("damage_cost")

##calculate mean lag per country
combined<-merge(combined, sr_tab, 'code')
socioeco_dat<-readRDS('soc_econ_country.rds') # From Sardain, Leung et al. Nature Sustainability
socioeco_dat<-subset(socioeco_dat, yr==2014)
socioeco_dat<-subset(socioeco_dat, IHS.i%in%countrycode(unique(expanded$destin_code), 'iso3c', 'country.name'))
socioeco_dat<-subset(socioeco_dat, IHS.j%in%countrycode(unique(expanded$destin_code), 'iso3c', 'country.name'))

## summarize damage and management spending per country by time period (ten)