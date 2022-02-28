## Code to add additional countries from Invacost v4.1
## While manually filled out, we can append the extra countries to the original reference sheet
## Dat Nguyen: 01-02-2022

rm(list=ls());gc()
graphics.off()


#### BODY ####

# Read in .csv
ref = read.csv("country-origin-ref_DN.csv", stringsAsFactors = F)
invacost = read.csv("invacost_origin_expanded_DN_fourpointone.csv", stringsAsFactors = F)

# Generate the files (for manual filling)
if(0){
  # Subset to relevant columns
  invacost2 = unique(invacost[,c("Official_country", "Geographic_region", "Geographic_region2", "Geographic_region2.code")])
  # Subset to ones we haven't seen yet
  invacost2 = invacost2[is.na(invacost2$Geographic_region2.code),]
  out = rbind(ref, invacost2[,c(1,2,3)])
  
  write.csv(out, "country-origin-ref_4p1_DN.csv", row.names = F)
}else{
  out = read.csv("country-origin-ref_4p1_DN.csv", stringsAsFactors = F)
}

# Fill in invacost
wh = which(is.na(invacost$Geographic_region2))
invacost$new = ifelse(is.na(invacost$Geographic_region2), 1, 0)
for(i in wh){
  print(i)
  wh2 = which(paste0(out$Geographic_region, out$Official_country) == 
                paste0(invacost$Geographic_region[i], invacost$Official_country[i]))
  invacost$Geographic_region2[i] = out$Geographic_region2[wh2]
  # Assign code
  invacost$Geographic_region2.code[i] = gsub("Europe","EUR", invacost$Geographic_region2[i])
  invacost$Geographic_region2.code[i] = gsub("Africa","AF", invacost$Geographic_region2.code[i])
  invacost$Geographic_region2.code[i] = gsub("Oceania","OC", invacost$Geographic_region2.code[i])
  invacost$Geographic_region2.code[i] =  gsub("South America","SA", invacost$Geographic_region2.code[i])
  invacost$Geographic_region2.code[i] = gsub("North America","NAm", invacost$Geographic_region2.code[i])
  invacost$Geographic_region2.code[i] = gsub("Asia","AS", invacost$Geographic_region2.code[i])
}
write.csv(invacost, "invacost_origin_expanded_fourpointone_DN2.csv", row.names = F)
