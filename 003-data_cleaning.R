require(invacost)
require(dplyr)
require(countrycode)
data(invacost)

origin<-read.csv('givers_takers_doublecheck.csv')
origin<-subset(origin, Continents !="UNK")
origin<-subset(origin, grepl("spp", origin$Species)==F)
##Correct for some remaining islands and unrecognized ISO3C countries
origin$Countries<-gsub("Galapagos", "Chile",origin$Countries )
origin$Countries<-gsub("Canary Is\\.", "Spain",origin$Countries )
origin$Countries<-gsub("Canary Is\\.", "Spain",origin$Countries )
origin$Countries<-gsub("Kosovo", "Serbia",origin$Countries )
origin$Countries<-gsub("Azores", "Portugal",origin$Countries )
origin$Countries<-gsub("Tibet", "China",origin$Countries )
origin$Countries<-gsub("Tadjikistan", "Tajikistan",origin$Countries )
origin$Countries<-gsub("Baleares", "Spain",origin$Countries )
origin$Countries<-gsub("Guiana", "France",origin$Countries )

origin$Countries<-gsub("Yugoslavia", "Bosnia and Herzegovina, Croatia, Macedonia, Montenegro, Serbia, Slovenia", origin$Countries )
origin$Countries<-gsub("ex-yugoslavia", "Bosnia and Herzegovina, Croatia, Macedonia, Montenegro, Serbia, Slovenia",origin$Countries )
origin$Countries<-gsub("Republic of Yugoslavia", "Bosnia and Herzegovina, Croatia, Macedonia, Montenegro, Serbia, Slovenia",origin$Countries )
#remove double spaces
origin$Countries<-gsub("\\s\\s", "",origin$Countries)
origin$Continents<-gsub("\\s\\s", "",origin$Continents)

countrylevel<-read.csv('species_country_origins.csv')
countrylevel[,2:231]<-0

countries<-setNames(matrix(0,nrow=nrow(origin),ncol= 230), names(countrylevel)[2:231])
colnames(countries)<-colnames(countrylevel)[2:231]
for (i in 1:nrow(origin))
{
  matched<-match(countrycode(strsplit(origin$Countries[i], split=c(";|\\,|\\,\\s|\\s;|;\\s"))[[1]], 'country.name', 'iso3c'), colnames(countries))
  matched<-unique(matched[is.na(matched)==F])
  countries[i,matched]<-1
}
continents<-data.frame(EUR=0, NAm=0, AS=0, AF=0, OC=0, SA=0)
for (i in 1:nrow(origin))
{
  matched<-match(strsplit(origin$Continents[i], split=c(";|\\,|\\,\\s|\\s;|;\\s"))[[1]], colnames(continents))
  matched<-unique(matched[is.na(matched)==F])
  continents[i,matched]<-1
}
continents[is.na(continents)]<-0

origin<-cbind(origin, continents, countries)
write.csv(origin,'allorigin_data.csv', row.names=F)
origin<-read.csv('allorigin_data.csv')
length(unique(invacost$Reference_ID))
length(unique(invacost$Cost_ID))
length(unique(invacost$Species))
invacost<-subset(invacost, Method_reliability=="High")
invacost<-subset(invacost, Implementation=="Observed")
invacost<-subset(invacost, grepl("Unit", invacost$Spatial_scale)==F)
invacost<-subset(invacost, Geographic_region!="Diverse/Unspecified")
invacost<-invacost[,c(1,3,7,8,12:21,27:31,36,37,45,46,52:55)]
origin<-subset(origin, Disease.Agent..to.remove.!=1)
origin<-subset(origin, is.na(origin$Domesticated..to.set.as.diverse.)==T)
invacost_origin<-merge(invacost, origin[,c(1:4,13:248)], by="Species")
write.csv(invacost_origin, file="origin_invacost_nonexpanded.csv", row.names=F)
invacost_origin<-subset(invacost_origin, is.na(invacost_origin$Probable_starting_year_adjusted)==F)
invacost_origin_expanded<-expandYearlyCosts(invacost_origin, 'Probable_starting_year_adjusted', "Probable_ending_year_adjusted")
write.csv(invacost_origin_expanded, file='invacost_origin_expanded.csv', row.names=F)


length(unique(invacost_origin_expanded$Cost_ID))
length(unique(invacost_origin_expanded$Reference_ID))
length(unique(invacost_origin_expanded$Species))
length(unique(invacost_origin_expanded$Official_country))
length(which(colSums(invacost_origin_expanded[,37:266])>0))


