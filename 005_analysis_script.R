rm(list=ls())

library(dplyr)
library(tidyr)

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

### BASIC ANALYSES

aggregate(mil~Destin_cont,data=expanded,FUN=sum)
aggregate(mil~Origin_cont,data=expanded,FUN=sum)

aggregate(mil~Origin_cont*Destin_cont,data=expanded,FUN=sum)

