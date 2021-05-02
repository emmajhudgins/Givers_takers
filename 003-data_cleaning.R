require(invacost)
require(dplyr)
require(countrycode)
require(here)
setwd(paste0(here(),'/data/'))
data(invacost)
island_nations<-read.csv('island_nations.csv')
origin<-read.csv('givers_takers_doublecheck.csv')
origin<-subset(origin, Continents !="UNK")
origin<-subset(origin, grepl("spp", origin$Species)==F)
##Correct for some remaining unrecognized ISO3C countries, Emma updated such that any overseas territory that is closer to another continent has its own country code and is affiliated with the closer continent.
origin$Countries<-gsub("Baleares", "Spain",origin$Countries )
origin$Countries<-gsub("Is\\.", "Islands",origin$Countries )
origin$Countries<-gsub("Leeward Islands", "Guadeloupe; Antigua and Barbuda; Saint Kitts and Nevis; Saint Martin; Virgin Islands",origin$Countries)
origin$Countries<-gsub("Laccadive Islands", "India",origin$Countries)
origin$Countries<-gsub("Windward Islands", "Dominica; Saint Lucia; Saint Vincent; Grenada",origin$Countries)
origin$Countries<-gsub("East Aegean Islands", "",origin$Countries)
origin$Countries<-gsub("Netherlands Antilles", "Bonaire, Curacao, Saba, Sint Eustatius, Sint Maarten",origin$Countries)
origin$Countries<-gsub("East Aegean Islands", "Greece",origin$Countries)
origin$Countries<-gsub("Corse", "France",origin$Countries)
origin$Countries<-gsub("Kriti", "Greece",origin$Countries)
origin$Countries<-gsub("Kuril Islands", "Russia",origin$Countries)
origin$Countries<-gsub("Andaman Islands", "India",origin$Countries)
origin$Countries<-gsub("Nicobar Islands", "India",origin$Countries)
origin$Countries<-gsub("Galapagos", "Chile",origin$Countries )
origin$Countries<-gsub("Kosovo", "Serbia",origin$Countries )
origin$Countries<-gsub("Azores", "Portugal",origin$Countries )
origin$Countries<-gsub("Tibet", "China",origin$Countries )
origin$Countries<-gsub("Tadjikistan", "Tajikistan",origin$Countries )
origin$Countries<-gsub("Malaya", "Singapore;Philippines;Myanmar;Malaysia",origin$Countries )

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
island_mat<-matrix(0, nrow(countries), length(unique(island_nations$New.countrycode)))
colnames(island_mat)<-unique(island_nations$New.countrycode)
countries<-cbind(countries, island_mat)
for (i in 1:nrow(origin))
{
  matched<-island_nations$New.countrycode[match(strsplit(origin$Countries[i], split=c(";|\\,|\\,\\s|\\s;|;\\s"))[[1]], island_nations$Unmatched.territories)]
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
for (i in 1:nrow(origin))
{
  matched<-island_nations$associated.continent[match(strsplit(origin$Countries[i], split=c(";|\\,|\\,\\s|\\s;|;\\s"))[[1]], island_nations$Unmatched.territories)]
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
invacost_origin<-merge(invacost, origin[,c(1:4,13:262)], by="Species")
write.csv(invacost_origin, file="origin_invacost_nonexpanded.csv", row.names=F)
invacost_origin<-subset(invacost_origin, is.na(invacost_origin$Probable_starting_year_adjusted)==F)
invacost_origin_expanded<-expandYearlyCosts(invacost_origin, 'Probable_starting_year_adjusted', "Probable_ending_year_adjusted")
write.csv(invacost_origin_expanded, file='invacost_origin_expanded.csv', row.names=F)


length(unique(invacost_origin_expanded$Cost_ID))
length(unique(invacost_origin_expanded$Reference_ID))
length(unique(invacost_origin_expanded$Species))
length(unique(invacost_origin_expanded$Official_country))
length(which(colSums(invacost_origin_expanded[,37:281])>0))


