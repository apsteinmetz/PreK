library(choroplethr)
library(choroplethrZip)
library(acs)
library(stringr)
library(dplyr)
library(tm)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(cowplot)
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
#zip_choropleth_acs(tableId="B19301", zip_zoom=nyc_zips)
#zip_choropleth_acs(tableId="B19301", county_zoom=nyc_fips)
#zip_choropleth(inc,
#               county_zoom=nyc_fips,
#               title="2011 Per Capita Income",
#               legend="Income",
#               num_colors=8)

#nytax<-read.csv("NYTAX.csv")
#tax1<-nytax
#names(tax1)<-c("region","count","TotInc","value")
#tax1$region<-as.character(tax1$region)
#zip_choropleth(tax1,
#               county_zoom=nyc_fips,
#               title="2013 New York City AGI",
#               legend="AGI",
#               num_colors=8)

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

#joint<-left_join(agg,tax1,by="region")
#regr<-lm(value.y~value.x,joint)
#summary(regr)

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
print(sumDayLength)


sumSeats<-seats%>%group_by(zip)%>%summarise(count=n(),numSeats=sum(seats,na.rm=TRUE))
sumSeatsChor<-sumSeats
names(sumSeatsChor)<-c("region","value")
zip_choropleth(sumSeatsChor,zip_zoom = nyc_zips)

#make a meta table with population, seats, per-capita income and per capita seats
data("df_pop_zip")
sumPopChor<-df_pop_zip%>%filter(region %in% nyc_zips)%>%filter(value>0)
sumPop<-sumPopChor
names(sumPop)<-c("zip","pop")



#combine the variables
allData<-sumSeats%>%join(sumPop)%>%join(kids)%>%join(inc)%>%na.omit()
#get rid of airports
allData<-filter(allData,zip!=11371 & zip!=11430)

# add normalized seats per capita/kid
allData<-mutate(allData,seatsPer100Kids = numSeats/ kidsUnder3*100,seatsPerCapita=numSeats/pop)

#normalize income cohorts
#compute income quantiles
fn<-ecdf(allData$perCapitaIncome)
allData<-mutate(allData,incomeQuantile=fn(allData$perCapitaIncome))
#compute seatsPer100Kids quantiles
fn<-ecdf(allData$seatsPer100Kids)
allData<-mutate(allData,seatsQuantile=fn(allData$seatsPer100Kids))

#is there an obvious relationship between income and seats?
plot(allData$seatsPer100Kids,allData$perCapitaIncome)


# set bins for bi-variate plot
bins<-3
allData<-mutate(allData,seatsBin=cut2(seatsPer100Kids,g=bins,levels.mean = TRUE))
allData<-mutate(allData,incomeBin=cut2(perCapitaIncome,g=bins,levels.mean = TRUE))



# create a data frame exclusively for use in a chorpleth object
# contains only zips as "region" and income/seats crosstab as "value"
bvc_df<-allData
levels(bvc_df$seatsBin)<-bins:1
levels(bvc_df$incomeBin)<-bins:1
bvc_df<-transmute(bvc_df,region=zip,value=paste(seatsBin,'-',incomeBin,sep=''))
#use color scheme shown here http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#assumes 9 levels

bvc<-ZipChoropleth$new(bvc_df)
bvc$title<-"NYC Per Capita Income vs. Pre-K Seats Per Child Under 3"
bvc$legend<-"SeatsRank - IncomeRank"
#use color scheme shown here http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
bvColors=c("#be64ac","#8c62aa","#3b4994","#dfb0d6","#a5add3","#5698b9","#e8e8e8","#ace4e4","#5ac8c8")
bvc$ggplot_scale = scale_fill_manual(name="", values=bvColors, drop=FALSE)
bvc$set_zoom_zip(county_zoom=nyc_fips,state_zoom = NULL,msa_zoom = NULL,zip_zoom = NULL)
# further annotate plot in the ggplot2 environment
gg<-bvc$render()
gg<-gg+guides(fill = guide_legend(nrow = 3,override.aes = list(colour=NULL)))
gg<-gg + theme(legend.text=element_blank())
gg<-ggdraw(gg) + draw_text(text = "More Income -->",x=0.91,y=0.58)
gg<-ggdraw(gg) + draw_text(text = "More Seats -->",x=0.84,y=0.49,angle=270)

#create a plot that will be the legend itself
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=bvColors)
test<-test+theme(legend.position="none")
test<-test + theme(axis.title.x=element_text(size=rel(1),color=bvColors[3])) + xlab("More Income -->")
test<-test + theme(axis.title.y=element_text(size=rel(1),color=bvColors[3])) + ylab("More Seats -->")
test<-test+theme(axis.text=element_blank())
test<-test+theme(line=element_blank())
#put both plots on a grid
ggdraw()+ draw_plot(test,0.1,0.7,width=0.2,height=0.2) + draw_plot(gg)


#crosstab of number of zip codes in income and seat Bins
xtab<-table(allData$seatsBin,allData$incomeBin)
hm<-as.data.frame(xtab)
names(hm)<-c("SeatsPer100Kids","IncomePerCapita","Freq")


xtab
#show heatmap of crosstab
ggplot(hm, aes(SeatsPer100Kids, IncomePerCapita)) + geom_tile(aes(fill = Freq),colour = "white") +
       scale_fill_gradient(low = "white", high = "steelblue")

