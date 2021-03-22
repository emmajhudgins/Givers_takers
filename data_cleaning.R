require(invacost)
require(dplyr)
require(countrycode)
data(invacost)

origin<-read.csv('givers_takers_doublecheck.csv')
origin<-subset(origin, Continents !="UNK")
origin<-subset(origin, grepl("spp", origin$Species)==F)

countrylevel<-read.csv('species_country_origins.csv')
countrylevel[,2:231]<-0

countries<-setNames(matrix(0,nrow=nrow(origin),ncol= 230), names(countrylevel)[2:231])
colnames(countries)<-colnames(countrylevel)[2:231]
for (i in 1:nrow(origin))
{
  matched<-match(countrycode(strsplit(origin$Countries[i], split=list(";"))[[1]], 'country.name', 'iso3c'), colnames(countries))
  matched<-unique(matched[is.na(matched)==F])
  countries[i,matched]<-1
}
continents<-data.frame(EUR=0, NAm=0, AS=0, AF=0, OC=0, SA=0)
for (i in 1:nrow(origin))
{
  matched<-match(strsplit(origin$Continents[i], split=list(";"))[[1]], colnames(continents))
  matched<-unique(matched[is.na(matched)==F])
  continents[i,matched]<-1
}
continents[is.na(continents)]<-0

origin<-cbind(origin, continents, countries)
write.csv(origin,'allorigin_data.csv', row.names=F)
invacost<-subset(invacost, Method_reliability=="High")
invacost<-subset(invacost, Implementation=="Observed")
invacost<-subset(invacost, Spatial_scale%in%c("Country", "Site"))
invacost<-invacost[,c(1,3,7,8,12:21,27:31,36,37,45,46,52:55)]
invacost_origin<-merge(invacost, origin, by="Species", all.x=T)
write.csv(invacost_origin, file="origin_invacost_nonexpanded.csv", row.names=F)
invacost_origin<-subset(invacost_origin, is.na(invacost_origin$Probable_starting_year_adjusted)==F)
invacost_origin_expanded<-expandYearlyCosts(invacost_origin, 'Probable_starting_year_adjusted', "Probable_ending_year_adjusted")

write.csv(invacost_origin_expanded, file='invacost_origin_expanded.csv', row.names=F)
