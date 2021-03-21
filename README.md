# Process and description of files and scripts used to get native ranges
# written by Dat Nguyen and Emma J Hudgins

## SCRIPTS ##
get_range_EH.R
  - provides the functions getRangeGISD, getRangeCABI and trim to retrieve dataset

native_ranges_countriesnew.R
  - using all invacost species in invacost package, attempts to download the ranges 
  - also checks whether it has an accepted name or synonyms and attempts the process on those as well

see our extended InvaCost region-to-country designation:
  - 6 unique cases were manually mapped:
    > kosovo = "Europe"
    > tropical america = "Central America"
    > pacific northwest = "Asia"
    > north america = "North America"
    > atlantic coast (north america) = "North America"
    > indian ocean - western = "Africa"

data_cleaning.R
- joins double-checked origin list (givers_takers_doublecheck.csv) to invacost data

data_analysis.R
- performs analyses of the manuscript.

## FILES ##

Datasheet_201911240709.csv
  - Full list of species with datasheets from CABI
  - Used in get_native.R to supplement the downloading process, to reduce error as getRangeCABI would have to Google the species without a match

region-country-designation-DN.csv
- reference for continent designation by country and iso3c code

givers_takers_doublecheck.csv
- result of manual double checking of web scraped origin data

## OUTPUT ##
tax_accepted.rds and tax_synonyms.rds
  - output from native_ranges_countriesnew.R
  - list of accepted names and synonyms for species that were originally missing from first pass using invacost species names

species-list.csv
  - output from native_ranges_countriesnew.R
  - the list of species with ranges that we found
  - provides the name, source and any syn onyms used to get ranges

speciesRanges.rds
  - output from get_native.R
  - R named list of species providing native ranges from CABI and GISD

origin_invacost_nonexpanded.csv
- output from data_cleaning.R, join of double-checked origin data with invacost

origin_invacost_expanded.csv
- output from performing expandYearlyCosts() on the nonexpanded data
