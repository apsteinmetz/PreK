library(choroplethr)
library(choroplethrZip)
library(acs)
library(stringr)
# compare Manhattan's Lower East Side and Upper East Side
manhattan_les = c("10002", "10003", "10009")
manhattan_ues = c("10021", "10028", "10044", "10128")
zip_choropleth_acs("B19301", num_colors=1, zip_zoom=c(manhattan_les, manhattan_ues))
# zoom in on all ZCTAs in the 5 counties (boroughs) of New York City
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
acs.code.percapitainc<-"B19301"
acs.code.pop<-"B01301"

zip_choropleth_acs(acs.code.percapitainc, endyear=2013,span=5,county_zoom=nyc_fips)

income.data=acs.fetch(geo=geo.make(state="NY",
                                   county=c("Kings","Queens","Richmond","New York","Bronx"), county.subdivision ="*"), table.number="B19301")


pke<-read.csv("pre_k_expansion.csv",header = FALSE)
lapply(pke,str_extract," \\d{2} (New|Add)")
lapply(pke,str_extract,"\\d{5}")
