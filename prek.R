library(choroplethr)
library(choroplethrZip)
library(acs)
library(stringr)
library(dplyr)
library(tm)
#census api key
#api.key.install("your key here")

## zoom in on all ZCTAs in the 5 counties (boroughs) of New York City
#acs.code.percapitainc<-"B19301"
#acs.code.pop<-"B01301"
#zip_choropleth_acs(acs.code.percapitainc, endyear=2013,span=5,county_zoom=nyc_fips)
#income.data=acs.fetch(geo=geo.make(state="NY",county=c("Kings","Queens","Richmond","New York","Bronx"), county.subdivision ="*"), table.number="B19301")

#DOE pre-k directories
# http://schools.nyc.gov/NR/rdonlyres/1F829192-ABE8-4BE6-93B5-1A33A6CCC32E/0/2015PreKDirectoryManhattan.pdf
# http://schools.nyc.gov/NR/rdonlyres/5337838E-EBE8-479A-8AB5-616C135A4B3C/0/2015PreKDirectoryBronx.pdf
# http://schools.nyc.gov/NR/rdonlyres/F2D95BF9-553A-4B92-BEAA-785A2D6C0798/0/2015PreKDirectoryBrooklyn.pdf
# http://schools.nyc.gov/NR/rdonlyres/B9B2080A-0121-4C73-AF4A-45CBC3E28CA3/0/2015PreKDirectoryQueens.pdf
# http://schools.nyc.gov/NR/rdonlyres/4DE31FBF-DA0D-4628-B709-F9A7421F7152/0/2015PreKDirectoryStatenIsland.pdf

uri = '2015PreKDirectoryManhattan.pdf'

pkdMan = readPDF(control = list(text = "-layout"))(elem = list(uri = uri),
                                                language = "en", id = "id1")   


# NYC county codes
nyc_fips = c(36085,36005, 36047, 36061, 36081)
#get the zips for all nyc counties
data("zip.regions")
nyc_zips<-data.frame(county.fips.numeric=nyc_fips)%>%inner_join(zip.regions)%>%select(region)%>%t
# make an ACS geo set
nycgeo<- geo.make(zip.code = nyc_zips,check =T)
income<-acs.fetch(endyear=2011,geography=nycgeo,table.number="B19301")
#put acs data in a form for rendering
inc<-cbind(geography(income),estimate(income))
names(inc)<-c("NAME","zip","perCapitaIncome")
#needs some cleanup of dupes. I don't know why
inc<-distinct(select(inc,zip,perCapitaIncome))

kidsUnder3<-acs.fetch(endyear=2011,geography=nycgeo,table.number="B09001",keyword = "Under 3")
kids<-cbind(geography(kidsUnder3),estimate(kidsUnder3))
names(kids)<-c("NAME","zip","kidsUnder3")
kids<-distinct(select(kids,zip,kidsUnder3))

#zip_choropleth_acs(tableId="B09001", zip_zoom=nyc_zips)


#the following are equivalent
zip_choropleth_acs(tableId="B19301", zip_zoom=nyc_zips)
zip_choropleth_acs(tableId="B19301", county_zoom=nyc_fips)
zip_choropleth(inc,
               county_zoom=nyc_fips,
               title="2011 Per Capita Income",
               legend="Income",
               num_colors=8)

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
tokens<-"(\\d+) (\\d{2}[A-Z]\\d{3}) (.+) (\\d{5}) (\\d+) ([a-zA-Z -]+) - - (\\d+) (\\d+)"

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

#extract zips and seats from text files made from PDFs using PDFTOTEXT.EXE (XPDF)
pkseattokens <-"(Address: )([.[:alnum:]- ()]+),+ ([0-9]{5})([a-zA-Z .()-:]+) ([0-9]{1,4}) (FD|HD|AM|PM|5H)"

txt1<-scan("bronx.txt",what="character",sep="\n")
txt2<-txt1[grep("^ +Address",txt1)]
txt2<-sub("'","",txt2)
bronx<-cbind(borough="Bronx",as.data.frame(str_match(txt2,pkseattokens))[,c(4,6,7)])
txt1<-scan("manhattan.txt",what="character",sep="\n")
txt2<-txt1[grep("^ +Address",txt1)]
txt2<-sub("'","",txt2)
manhattan<-cbind(borough="Manhattan",as.data.frame(str_match(txt2,pkseattokens))[,c(4,6,7)])
txt1<-scan("brooklyn.txt",what="character",sep="\n")
txt2<-txt1[grep("^ +Address",txt1)]
txt2<-sub("'","",txt2)
brooklyn<-cbind(borough="Brooklyn",as.data.frame(str_match(txt2,pkseattokens))[,c(4,6,7)])
txt1<-scan("queens.txt",what="character",sep="\n")
txt2<-txt1[grep("^ +Address",txt1)]
txt2<-sub("'","",txt2)
queens<-cbind(borough="Queens",as.data.frame(str_match(txt2,pkseattokens))[,c(4,6,7)])
txt1<-scan("staten.txt",what="character",sep="\n")
txt2<-txt1[grep("^ +Address",txt1)]
txt2<-sub("'","",txt2)
staten<-cbind(borough="Staten Island",as.data.frame(str_match(txt2,pkseattokens))[,c(4,6,7)])

#merge nyc together
seats<-rbind(manhattan,bronx)
seats<-rbind(seats,queens)
seats<-rbind(seats,brooklyn)
seats<-rbind(seats,staten)
names(seats)<-c("borough","zip","seats","daylength")
seats$seats<-as.integer(seats$seats)

sumDayLength<-seats%>%group_by(daylength)%>%summarise(NumSchools=n(),NumSeats=sum(seats,na.rm=TRUE))
print(SumDayLength)


sumSeats<-seats%>%group_by(zip)%>%summarise(count=n(),numSeats=sum(seats,na.rm=TRUE))
sumSeatsChor<-sumSeats
names(sumSeatsChor)<-c("region","value")
#zip_choropleth(SumSeatsChor,zip_zoom = nyc_zips)

#make a meta table with population, seats, per-capita income and per capita seats
data("df_pop_zip")
sumPopChor<-df_pop_zip%>%filter(region %in% nyc_zips)%>%filter(value>0)
sumPop<-sumPopChor
names(sumPop)<-c("zip","pop")
allData<-sumSeats%>%join(sumPop)%>%join(kids)%>%join(inc)%>%na.omit()
allData<-mutate(allData,seatsPerKid = numSeats/ kidsUnder3,seatsPerCapita=numSeats/pop)
plot(allData$seatsPerKid,allData$perCapitaIncome)


