_Repo to accompany_
# The ecological dimension of global trade: origin and recipient regions of biological invasion costs

###  Emma J. Hudgins*, Ross N. Cuthbert*, Phillip J. Haubrock*, Nigel G. Taylor, Melina Kourantidou, Dat Nguyen, Alok Bang, Anna J. Turbelin, Desika Moodley, Elizabeta Briski, Syrmalenia G. Kotronaki, Franck Courchamp

> code written by Dat Nguyen and Emma J Hudgins

## SCRIPTS ##
001-get_range_EH.R
  - provides the functions getRangeGISD, getRangeCABI and trim to retrieve dataset

002-native_ranges_countries_fourpointone.R
  - using all invacost species in invacost package (version 4.1), attempts to download the ranges 
  - also checks whether it has an accepted name or synonyms and attempts the process on those as well

see our extended InvaCost region-to-country designation:
  - 6 unique cases were manually mapped:
    > kosovo = "Europe"  
    > tropical america = "Central America"  
    > pacific northwest = "Asia"  
    > north america = "North America"  
    > atlantic coast (north america) = "North America"  
    > indian ocean - western = "Africa"  

003-data_cleaning_fourpointone.R
- joins double-checked origin list (givers_takers_doublecheck_.csv) to invacost data

004-assign_con_DN_fourpointone.R
- standardizes continent designations with pre-defined list 

0041-add4p1Countries.R
- additional code to add continent designations for new data entries not present in the first version of our analysis (Invacost 3.0)

005_analysis_script.R
- performs analyses of the manuscript at the continent level (Figs 1-2)

006_countrylevel_script.R
- performs country-level analyses of the manuscript (Figs 3-4)

## KEY FILES ##
*see also old data folder for Invacost 3.0 files*

Datasheet_201911240709.csv
  - Full list of species with datasheets from CABI
  - Used in get_native.R to supplement the downloading process, to reduce error as getRangeCABI would have to Google the species without a match

country-origin-ref_4p1_DN.csv
- reference for continent designation by country and iso3c code

givers_takers_doublecheck_fourpointone.csv
- result of manual double checking of web scraped origin data

Gravity_V202102.Rds
- CEPII Gravity database as of February 2022

island.nations.csv
- pre-determined continent designations for island nations

Invacost_database_v4.1.csv
- equivalent to calling data(invacost) from the invacost package with the version argument set to 4.1

invacost_origin_expanded_fourpointone_DN2.csv
- expanded invacost database joined to origin country designations

soc_econ_country.rds
- socioeconomic predictors from Sardain et al. 2019

sTwist_database.csv
- IAS first record and country occurrence information from Seebens et al. (2022)

turbelinetal_2017_IAS
- IAS load for each country by iso3c code from Turbelin et al. 2017

## KEY OUTPUT ##

tax_accepted_fourpointone.rds and tax_synonyms_fourpointone.rds
  - output from native_ranges_countriesnew.R
  - list of accepted names and synonyms for species that were originally missing from first pass using invacost species names

trade_averaged.RDS, trade_historical.RDS
- average yearly pairwise trade for 21st and 20th century

senders.RDS, receivers.RDS, top_pairs.RDS
- flows at country level

given.csv.RDS, received.csv//RS
- flows at continent level

invacost_givers_data.csv/.RDS, invacost_takers_data.csv/.RDS
- decadal flows at continent level