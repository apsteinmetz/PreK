library(choroplethr)
library(choroplethrZip)
library(acs)
library(stringr)
library(dplyr)
# compare Manhattan's Lower East Side and Upper East Side
manhattan_les = c("10002", "10003", "10009")
manhattan_ues = c("10021", "10028", "10044", "10128")
#zip_choropleth_acs("B19301", num_colors=1, zip_zoom=c(manhattan_les, manhattan_ues))
## zoom in on all ZCTAs in the 5 counties (boroughs) of New York City
#acs.code.percapitainc<-"B19301"
#acs.code.pop<-"B01301"
#zip_choropleth_acs(acs.code.percapitainc, endyear=2013,span=5,county_zoom=nyc_fips)
#income.data=acs.fetch(geo=geo.make(state="NY",county=c("Kings","Queens","Richmond","New York","Bronx"), county.subdivision ="*"), table.number="B19301")

nyc_fips = c(36005, 36047, 36061, 36081, 36085)

nytax<-read.csv("NYTAX.csv")
tax1<-nytax
names(tax1)<-c("region","count","TotInc","value")
tax1$region<-as.character(tax1$region)
zip_choropleth(tax1,
               county_zoom=nyc_fips,
               title="2013 New York City AGI",
               legend="AGI",
               num_colors=8)

pke<-read.csv("pre_k_expansion.csv",header = FALSE)
tokens<-"(\\d+) (\\d{2}[A-Z]\\d{3}) (.+) (\\d{5}) (\\d+) ([a-zA-Z -]+)(\\d+) (\\d+)"

pke1<-apply(X=pke,MARGIN=1,FUN=str_match,tokens)
pke2<-as.data.frame(t(pke1))[,c(-1,-2,-9)]
header<-c("District","Address","Zip","Add","Note","Total")
names(pke2)<-header
pke2$Add<-as.numeric(levels(pke2$Add)[pke2$Add])
pke2$Total<-as.numeric(levels(pke2$Total)[pke2$Total])
byzip<-group_by(pke2,Zip)
agg<-summarise(byzip,value=sum(Total,na.rm=TRUE))
names(agg)<-c("region","value")
zip_choropleth(agg,
  county_zoom=nyc_fips,
  title="2014 New York City Pre-K Seats added",
  legend="New Seats")

joint<-left_join(agg,tax1,by="region")
regr<-lm(value.y~value.x,joint)
summary(regr)
