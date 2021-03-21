# Scraping functions that provide the native and exotic range for species
# Dat Nguyen; February 4, 2021

require(rvest)

##-----------------##
##### FUNCTIONS #####

#' @name = taxonomic name of the species
getRangeGISD = function(name){
  # as a first attempt, trying inputting the species directly into the url of iucngisd
  cat("\tDownloading via IUCN GISD\n")
  test = paste("http://www.iucngisd.org/gisd/speciesname/",gsub("\\s+","+",name),sep = "")
  gisdpage = read_html(test)
  text = html_text(gisdpage)
  
  # if found, then collect the range data
  if(grepl("Countries.*with distribution records",text)){
    # get native range using nr-col node
    t = as.character(html_nodes(gisdpage, "#nr-col"))
    t = unlist(strsplit(t, "(</li>)|(<li>)"))
    nr = t[c(F,T)]
 
    output = trim(nr)
    
  }else{
    cat("NOT FOUND\n")
    # notfound!
    output =NA
  }
  
  return(output)
}

# You can input the datasheet directly into the function to make scraping faster
getRangeCABI = function(name, link = NA){
  cat("\tDownloading via CABI ISC\n")
  
  # Google it if we don't have a CABI link
  if(is.na(link)){
    st = paste(name, "CABI")
    st = gsub("\\s+","+",st)
    gpage = read_html(paste("https://www.google.ca/search?q=",st,sep=""))
    glinks = html_attr(html_nodes(gpage,"a"),"href")
    glinks = glinks[grepl("cabi.org/isc",glinks)]
    
    if(length(glinks)==0){
      cat("GPAGE: NOT FOUND\n")
      # if not found in GISD as well, then just return NA
      return(NA)
    }else{
      # format to proper url
      l1 = URLdecode(glinks[1])
      l1 = gsub("/url\\?q=","",l1)
      l1 = gsub("&sa.*","",l1)
      
      # read url
      cabipage = read_html(l1)
      
      txt = html_text(cabipage)
      
      # check if it's the correct species!
      sss = sum(lengths(regmatches(txt, gregexpr(name, txt))))
      
      # check if it's the correct species!
      # The ruleset is if the name of the species occurs more than once, it is correct
      if(sss > 1 & grepl("Distribution Table", txt)){
        cat("\t\tFound! Scraping...\n")        
      }else{
        cat("MISMATCH: NOT FOUND\n")
        return(NA)
    }}
  }else{
    cabipage = tryCatch(read_html(link), error = function(e){NA})
  }
  
  if(is.na(cabipage)){
    cat("\tPAGE ERROR\n")
    return(NA)
  }
  
  # and tables
  tables = html_table(cabipage, fill = T)
  
  # determine which one we need using column names
  # lapply goes through the column names and if "Origin" and "Invasive" are in it then return F
  table = which(unlist(lapply(tables, function(x){
    if("Origin" %in% colnames(x)){
      return(T)
    }else{
      return(F)
    }
  }))==T)
  
  # if can't find the table, then it's broken
  if(length(table)==0){
    cat("TABLE MISSING\n")
    return( NA)
  }
  
  table = tables[[table]]
  
  # assign country to each region
  table$country = unlist(lapply(1:nrow(table), function(x){
    return(table$`Continent/Country/Region`[max(which(!grepl("^-",table$`Continent/Country/Region`[1:x])))])
  }))
  

  if(nrow(table) == 0 | all(table$Origin == "")){
    cat("TABLE: NO DATA\n")
    return(NA)
  }
  
  
  nr = table[which(table$Origin == 'Native' & !grepl("^-",table[,1])),1]
  # Add the extra countries
  # In cases where a country is blank (== ""), but all of its subregions are either Native or Introduced
  table2 = table[!(table$country %in% nr) & table$Origin != "",]
  if(nrow(table2) > 0){
    cc = unique(table2$country)
    for(c in cc){
      temp = table2$Origin[table2$country==c]
      if(any(temp == "Native")) nr = c(nr, c)
    }
  }
  
  if(length(nr) == 0) nr = NA

  output =  nr
  
  return(output)
}

