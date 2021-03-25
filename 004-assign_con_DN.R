## This code re-assigns the InvaCost dataset and recipient continents
# to match the native range content designations

rm(list=ls())

library(countrycode)

# Read in data
ic_ori = read.csv("invacost_origin_expanded.csv", stringsAsFactors = F)

# We only care about the country-level, politically
if(0){
  ori1 = unique(ic_ori[,c("Geographic_region", "Official_country")])
  
  ori1$Geographic_region2 = NA
  write.csv(ori1, "./country-origin-ref_DN-RAW.csv", row.names = F) 
}else{
  ori1 = read.csv("./country-origin-ref_DN.csv", stringsAsFactors = F)
}

ic_ori$Geographic_region2 = ori1$Geographic_region2[match(paste(ic_ori$Geographic_region, ic_ori$Official_country),
                                                          paste(ori1$Geographic_region, ori1$Official_country))]
# Add the same identifiers as well though
ic_ori$Geographic_region2.code = ic_ori$Geographic_region2
ic_ori$Geographic_region2.code = gsub("North America","NAm",ic_ori$Geographic_region2.code)
ic_ori$Geographic_region2.code = gsub("South America","SA",ic_ori$Geographic_region2.code)
ic_ori$Geographic_region2.code = gsub("Oceania","OC",ic_ori$Geographic_region2.code)
ic_ori$Geographic_region2.code = gsub("Asia","AS",ic_ori$Geographic_region2.code)
ic_ori$Geographic_region2.code = gsub("Africa","AF",ic_ori$Geographic_region2.code)
ic_ori$Geographic_region2.code = gsub("Europe","EUR",ic_ori$Geographic_region2.code)

write.csv(ic_ori, "invacost_origin_expanded_DN.csv", row.names = F)

