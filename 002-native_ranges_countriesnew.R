library(glue)
library(countrycode)
library(invacost)
library(taxize)
library(rgbif)

source('../001-get_range_EH.R') #requires rvest package
CABI_list = read.csv("Datasheet_201911240709.csv", stringsAsFactors = F)
data(invacost)
##2022 update
invacost<-read.csv('new_species_4.1.csv')
species<-unique(invacost$Species)
ranges<-list()
# syns = list()
# acc = list()
# for(i in 1:length(species)){
#     sp = species[i]
#    tryCatch({tsn=get_tsn(sp, messages = F)}, error=function(e) {tsn=NA})
#     if(!is.na(tsn)){
#       acc[[sp]] = itis_acceptname(tsn)
#       syns[[sp]] = synonyms(tsn, db = "itis")[[1]]
#     }}
#   saveRDS(syns, "tax_synonyms.rds")
#   saveRDS(acc, "tax_accepted.rds")
new_species<-species
for (i in 1:length(new_species))
{
  ranges[[i]]<-tryCatch(getRangeCABI(new_species[i]), error=function(e){list(NA)}) 
  Sys.sleep(runif(1,5,10))# helps CABI not know you're a robot, maybe no longer needed
}

rangesGISD<-lapply(new_species, getRangeGISD)

# clean up some unrecognized countries - might need to expand the cases for a bigger species list
Serbias<-which(lapply(ranges, function(x){any(grepl("Serbia",x))})==T)
for (r in Serbias)
{
  ranges[[r]]<-c(ranges[[r]][-which(grepl("Serbia", ranges[[r]]))], "Serbia", "Montenegro")
}
Antilles<-which(lapply(ranges, function(x){any(grepl("Netherlands Antilles",x))})==T)
for (r in Antilles)
{
  ranges[[r]]<-c(ranges[[r]][-which(grepl("Netherlands Antilles", ranges[[r]]))], "Netherlands")
}
regions<- vector(mode = "list", length = length(unique(invacost$Species)))

regions[which(lapply(ranges,length)>0& sapply(ranges,function(x){any(is.na(x))})==F)]<-lapply(ranges[which(lapply(ranges,length)>0& sapply(ranges,function(x){any(is.na(x))})==F)], FUN=function(x){countrycode(unlist(x),origin='country.name',destination="iso3c")})


Serbias<-which(lapply(rangesGISD, function(x){any(grepl("serbia",x))})==T)
for (r in Serbias)
{
  rangesGISD[[r]]<-c(rangesGISD[[r]][-which(grepl("serbia", rangesGISD[[r]]))], "Serbia", "Montenegro")
}
ex_yug<-which(lapply(rangesGISD, function(x){any(grepl("yugoslavia",x))})==T)
for (r in ex_yug)
{
  rangesGISD[[r]]<-c(rangesGISD[[r]][-which(grepl("yugoslavia", rangesGISD[[r]]))], "Serbia", "Montenegro","Croatia", "Bosnia and Herzegovina", "Kosovo", "North Macedonia", "Slovenia") # Kosovo not recognized by iso3c
}
regionsGISD<- vector(mode = "list", length = length(unique(invacost$Species)))
regionsGISD[which(lapply(rangesGISD,length)>0& sapply(rangesGISD,function(x){any(is.na(x))})==F)]<-lapply(rangesGISD[which(lapply(rangesGISD,length)>0& sapply(rangesGISD,function(x){any(is.na(x))})==F)], FUN=function(x){countrycode(unlist(x),origin='country.name',destination="iso3c")}) # not including continent-level ranges at this stage

saveRDS(regions, file="ranges_fourpointone.rds")
saveRDS(regionsGISD, file="rangesGISD_fourpointone.rds")
combined_list<-mapply(c,regions, regionsGISD, SIMPLIFY=FALSE)
names(combined_list)<-unique(invacost$Species)
saveRDS(combined_list, file="species_country_list_fourpointone.rds")
# 
# regions<-readRDS('~/Downloads/ranges.rds')
# regionsGISD<-readRDS('~/Downloads/rangesGISD.rds')
##Associate with continents
 colnames(species_list)<-c("Species", paste((unique(unlist(regions)))))

 for (i in which(unique(invacost$Species)%in%new_species))
 {
   species_list[i,1]<-invacost$Species[i]
   j=which(new_species==unique(invacost$Species)[i])

   species_list[i,which(colnames(species_list)%in%regions[[j]])]<-1
   species_list[i,which(colnames(species_list)%in%regionsGISD[[j]])]<-1
 }
write.csv(species_list, file="species_country_origins_fourpointone.csv", row.names=F)

species_list<-read.csv('species_country_origins_fourpointone.csv')
species_list[,1]<-unique(invacost$Species)
missing<-unique(invacost$Species)[which(rowSums(species_list[,-1],na.rm=T)==0)]
write.csv(missing, file="missing_country_species_fourpointone.csv", row.names=F)
write.csv(species_list, file="initial_species_country_fourpointone.csv", row.names=F)
listform<-readRDS('species_country_list_fourpointone.rds')

species.list<-missing
syns = list()
acc = list()
for(i in 1:length(species.list)){
  sp = species.list[i]
  tsn = get_tsn(sp, messages = F)
  if(!is.na(tsn)){
    acc[[sp]] = itis_acceptname(tsn)
    syns[[sp]] = synonyms(tsn, db = "itis")[[1]]
  }
}
saveRDS(syns, "tax_synonyms_fourpointone.rds")
saveRDS(acc, "tax_accepted_fourpointone.rds")


spl = list()
species.list<-data.frame(Species=missing)
species.list$source =  NA # Get source of dataset
species.list$synonym = NA # If we end up using the synonym instead, note what their name was
for(i in 1:nrow(species.list)){
  sp = species.list$Species[i]
  cat(i,"-",sp,"\n")
  
  # If we didn't find the range, check the accepted name and synonyms
  sp2 = c()
  
  # Do we have an accepted name?
    if(sp %in% names(acc)){
      temp.acc = acc[[sp]]
      
      if(!is.na(temp.acc$acceptedname)){
        sp2 = c(sp2, temp.acc$acceptedname[1])
      }
    }
    # Check synonyms
    if(sp %in% names(syns)){
      temp.syn = syns[[sp]]
      if(nrow(temp.syn) > 0){
        sp2 = c(sp2, na.omit(temp.syn$syn_name))
      }
    }
    
    if(length(sp2)>0){
      # Apply the function to accepted name and all synonyms
      rG = lapply(sp2, FUN = function(x){getRangeGISD(x)})
      names(rG) = sp2
      
      rC = lapply(sp2, FUN = function(x){
        if(x %in% CABI_list$Scientific.name){
          wh = which(CABI_list$Scientific.name == x)
          return(getRangeCABI(sp, link = CABI_list$URL[wh]))
        }else{
          return(getRangeCABI(x))
        }
      })
      names(rC) = sp2
      
      # Subset to non-NA, if possible
      rG = rG[-which(unlist(lapply(rG, FUN = function(x){all(is.na(x))})))]
      rC = rC[-which(unlist(lapply(rC, FUN = function(x){all(is.na(x))})))]
      
      if(length(rG) > 0 | length(rC) > 0){
        species.list$synonym[i] = paste(unique(c(names(rG), names(rC))), collapse = ";")
        
        if(length(rG) > 0){
          rG = unique(unlist(rG))
        }else{
          rG = NA
        }
        if(length(rC) > 0){
          rC = unique(unlist(rC))
        }else{
          rG = NA
        }
      }
      
      nG = all(is.na(rG))
      nC = all(is.na(rC))
  }
  
  if(!nG | !nC){
    if(!nG) species.list$source[i] = "GISD"
    if(!nC) species.list$source[i] = paste(na.omit(c(species.list$source[i], "CABI")), collapse = ";")
    
    spl[[sp]] = unique(na.omit(c(rG, rC)))
  }
}


out = species.list[species.list$Species %in% names(spl),]
spl = lapply(spl, FUN=function(x){return(unique(tolower(x)))})

write.csv(out, "./output/species-list-fourpointone.csv", row.names = F)
saveRDS(spl, "./output/speciesRanges_fourpointone.rds")

for (i in 1:length(spl))
{
species_list[which(species_list$Species%in% names(spl)[i]),which(colnames(species_list)%in%countrycode(spl[[i]],origin='country.name',destination="iso3c"))]<-1
}
combined_list[which(species_list$Species%in% names(spl))]<-spl


saveRDS(combined_list, file="species_country_list_fourpointone.rds")
write.csv(species_list, file="species_country_origins_fourpointone.csv", row.names=F)


missing<-species_list$Species[which(rowSums(species_list[,-1], na.rm=T)==0)]
newspp<-list()
for (i in 1:length( species_list$Species)){
temp<-tryCatch(occ_search(scientificName= species_list$Species[i], establishmentMeans="NATIVE")$data, error=function(e){data.frame(NA)})
if(any(colnames(temp)=="continent")){
  if(any(colnames(temp)=="country")){
      newspp[[i]]<-unique.data.frame(temp[,c("continent", "country")])}else{  newspp[[i]]<-unique.data.frame(temp[,c("continent")])}
}
}

newspp_code<-lapply(newspp[which(lapply(newspp,length)>0)], FUN=function(x){countrycode(unlist(x),origin='country.name',destination="iso3c")})
names(newspp_code)<-species_list$Species[which(lapply(newspp,length)>0)]
newspp_code<-newspp_code[which(names(newspp_code)%in%missing)]


newspp_continent<-lapply(newspp[which(lapply(newspp,length)>0)], FUN=function(x){unique(x$continent)})
names(newspp_continent)<-species_list$Species[which(lapply(newspp,length)>0)]
newspp_continent<-newspp_continent[which(names(newspp_continent)%in%missing)]
for (i in 1:length(which(species_list$Species%in% names(newspp_code)))){
combined_list[[which(species_list$Species%in% names(newspp_code))[i]]]<-c(combined_list[[which(species_list$Species%in% names(newspp_code))[i]]],newspp_continent[[i]])}

for (i in 1:length(newspp_code))
{
  species_list[which(species_list$Species%in% names(newspp_code)[i]),which(colnames(species_list)%in%countrycode(newspp_code[[i]],origin='country.name',destination="iso3c"))]<-1
}
write.csv(species_list[which(rowSums(species_list[,-1], na.rm=T)>0),], file="species_country_origins_full_fourpointone.csv", row.names=F)
write.csv(species_list$Species[which(rowSums(species_list[,-1], na.rm=T)==0)], file="species_country_missing_fourpointone.csv", row.names=F)
saveRDS(combined_list, file="species_country_list_fourpointone.rds")
colSums(species_list[,-1], na.rm=T)[order(desc(colSums(species_list[,-1], na.rm=T)))]

species_list$Countries<-lapply(combined_list, FUN=function(x){paste(as.vector(x),collapse=";")})
species_list$Countries<-gsub('NA;', '', species_list$Countries)
species_list$Countries<-gsub('NA$', '', species_list$Countries)
write.csv(species_list, file="species_country_origins_complete_fourpointone.csv", row.names=F)
