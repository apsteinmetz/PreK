library(choroplethr)
#devtools::install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethrZip)
library(acs)
library(stringr)
library(dplyr)
library(Hmisc)
library(reshape2)
library(ggplot2)
#devtools::install_github("wilkelab/cowplot")
library(cowplot)

#census api key
#see acs package documentation
#api.key.install("your key here")

# -----------------------------------------------------------------------
# get census data on children and income
# NYC county codes
nyc_fips = c(36085,36005, 36047, 36061, 36081)
#get the zips for all nyc counties
data("zip.regions")
nyc_zips<-data.frame(county.fips.numeric=nyc_fips)%>%inner_join(zip.regions)%>%select(region)%>%t
# make an ACS geo set
nycgeo<- geo.make(zip.code = nyc_zips)

income<-acs.fetch(endyear=2011,geography=nycgeo,table.number="B19301")
#put acs data in a form for rendering
inc<-cbind(geography(income),estimate(income))
names(inc)<-c("NAME","zip","perCapitaIncome")
#needs some cleanup of dupes. I don't know why
inc<-distinct(select(inc,zip,perCapitaIncome))


#kids under 3 in 2011 should approximate Pre-K kids in 2015
kidsUnder3<-acs.fetch(endyear=2011,geography=nycgeo,table.number="B09001",keyword = "Under 3")
kids<-cbind(geography(kidsUnder3),estimate(kidsUnder3))
names(kids)<-c("NAME","zip","kidsUnder3")
kids<-distinct(select(kids,zip,kidsUnder3))
kids<-kids%>%select(zip,kidsUnder3)%>%distinct()%>%filter(kidsUnder3!=0 | kidsUnder3!=NA)

# get total population
totalPop<-acs.fetch(endyear=2011,geography=nycgeo,table.number="B01003")
pop<-cbind(geography(totalPop),estimate(totalPop))
names(pop)<-c("NAME","zip","totPop")
pop<-pop%>%select(zip,totPop)%>%distinct()%>%filter(totPop!=0)

#where are zips with the most rugrats?
kidsChor <- pop%>%inner_join(kids)%>%transmute(region=zip,value=kidsUnder3/totPop*100)
zip_choropleth(kidsChor,zip_zoom = nyc_zips,title = "Percentage of Kids Under 3 in 2011")

# if you want an underlying map and are using version 1.4 of zip_choropleth
#zip_choropleth(kidsChor,zip_zoom = nyc_zips,title = "Number of Pre-K Seats",reference_map = TRUE)

# -----------------------------------------------------------------------
# get NYC data on pre-K programs
# scan seat directory pdfs and put into a data frame by zip code
#DOE pre-k directories
urls<- c("http://schools.nyc.gov/NR/rdonlyres/1F829192-ABE8-4BE6-93B5-1A33A6CCC32E/0/2015PreKDirectoryManhattan.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/5337838E-EBE8-479A-8AB5-616C135A4B3C/0/2015PreKDirectoryBronx.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/F2D95BF9-553A-4B92-BEAA-785A2D6C0798/0/2015PreKDirectoryBrooklyn.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/B9B2080A-0121-4C73-AF4A-45CBC3E28CA3/0/2015PreKDirectoryQueens.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/4DE31FBF-DA0D-4628-B709-F9A7421F7152/0/2015PreKDirectoryStatenIsland.pdf")

exePath = "C:\\Users\\nsteinm\\Documents\\R\\"
exe <- paste(exePath,"pdftotext.exe",sep="")

#regex to parse address line
pkseattokens <-"(Address: )([.[:alnum:]- ()]+),+ ([0-9]{5})([a-zA-Z .()-:]+) ([0-9]{1,4}) (FD|HD|AM|PM|5H)"

# each of the PDF directories have 27 pages of intro material. Skip it. This might change for different years. Check PDFs
firstPage = 28

dests <- tempfile(str_match(urls,"Directory(\\w.+).pdf")[,2],fileext = ".pdf")
txt<- NULL
for (i in 1:length(urls)) {
  download.file(urls[i],destfile = dests[i],mode = "wb")
  # pdftotxt.exe is in current directory and convert pdf to text using "table" style at firstpage
  result<-system(paste(exe, "-table -f", firstPage, dests[i], sep = " "), intern=T)
  # get txt-file name and open it  
  filetxt <- sub(".pdf", ".txt", dests[i])
  txt <- append(txt,readLines(filetxt)) # don't mind warning..
}

# find address line which contains zip and seat count
txt2<-txt[grep("Address:",txt)]
# strip chars that will mess up regex
txt2<-sub("'","",txt2)
seats<-as.data.frame(str_match(txt2,pkseattokens))[,c(4,6,7)]
names(seats)<-c("zip","seats","daylength")
seats$seats<-as.integer(seats$seats)
# aggregate seat count by zip code
sumSeats<-seats%>%group_by(zip)%>%summarise(count=n(),numSeats=sum(seats,na.rm=TRUE))
names(sumSeats)<-c("zip","schools","numSeats")


#how do the programs break out in terms of day length?
sumDayLength<-seats%>%group_by(daylength)%>%summarise(NumSchools=n(),NumSeats=sum(seats,na.rm=TRUE))
print(sumDayLength)


# -----------------------------------------------------------------------
#combine the vectors for seats, income and population
allData<-sumSeats%>%join(pop)%>%join(kids)%>%join(inc)%>%na.omit()
#get rid of airports, JFK and LGA
allData<-filter(allData,zip!=11371 & zip!=11430)

# add normalized seats per capita/kid
allData<-mutate(allData,seatsPer100Kids = numSeats/ kidsUnder3*100,seatsPerCapita=numSeats/totPop)

#normalize income cohorts
#compute income quantiles
fn<-ecdf(allData$perCapitaIncome)
allData<-mutate(allData,incomeQuantile=fn(allData$perCapitaIncome))
#compute seatsPer100Kids quantiles
fn<-ecdf(allData$seatsPer100Kids)
allData<-mutate(allData,seatsQuantile=fn(allData$seatsPer100Kids))
# set bins for bi-variate plot
bins<-3
allData<-mutate(allData,seatsBin=cut2(seatsPer100Kids,g=bins,levels.mean = TRUE))
allData<-mutate(allData,incomeBin=cut2(perCapitaIncome,g=bins,levels.mean = TRUE))

#Write out allData so we don't have to scrape the city web data each time
# and we can make the analysis self-contained

write.csv(allData,file='PreKData.csv',row.names = FALSE)
# ------------------------------------------------------------------------
# end of web scraping and data prepping
# -------------------------------------------------------------------------

# load data to analyze
allData<-read.csv(file='PreKData.csv')


# some preliminary pictures
allData%>%transmute(region=zip,value=schools)%>%zip_choropleth(zip_zoom = nyc_zips,title = "Number of Schools")
allData%>%transmute(region=zip,value=numSeats)%>%zip_choropleth(zip_zoom = nyc_zips,title = "Number of Pre-K Seats")

#is there an obvious relationship between income and seats?
ggplot(allData,aes(y=seatsPer100Kids,x=perCapitaIncome))+geom_point()
#lets look at the same data normalized by quantile
#no obvious relationship
ggplot(allData,aes(y=seatsQuantile,x=incomeQuantile))+geom_point()


# create a data frame exclusively for use in a chorpleth object
# contains only zips as "region" and income/seats crosstab as "value"
bvc_df<-allData
levels(bvc_df$seatsBin)<-bins:1
levels(bvc_df$incomeBin)<-bins:1
bvc_df<-transmute(bvc_df,region=zip,value=paste(seatsBin,'-',incomeBin,sep=''))
title1<-"NYC Per Capita Income in 2011 vs. Pre-K Seats Per Child 3-5 in 2015"

#create choropleth object
bvc<-ZipChoropleth$new(bvc_df)
bvc$title<-title1
#use color scheme shown here http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#assumes 9 levels
bvColors=c("#be64ac","#8c62aa","#3b4994","#dfb0d6","#a5add3","#5698b9","#e8e8e8","#ace4e4","#5ac8c8")
bvc$ggplot_scale = scale_fill_manual(name="", values=bvColors, drop=FALSE)
bvc$set_zoom_zip(county_zoom=nyc_fips,state_zoom = NULL,msa_zoom = NULL,zip_zoom = NULL)
# further annotate plot in the ggplot2 environment
gg<-bvc$render() + theme(legend.position="none")

# create 3x3 legend Use cowplot package for labels
# deprecated in favor of custom rolled legend below
#gg<-gg+guides(fill = guide_legend(nrow = 3,override.aes = list(colour=NULL)))
#gg<-gg + theme(legend.text=element_blank())
#gg<-ggdraw(gg) + draw_text(text = "More Income -->",x=0.91,y=0.58)
#gg<-ggdraw(gg) + draw_text(text = "More Seats -->",x=0.84,y=0.49,angle=270)

#use cowplot to create a plot that will be the legend itself
#first create a legend plot
legendGoal=melt(matrix(1:9,nrow=3))
lg<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
lg<- lg + scale_fill_manual(name="",values=bvColors)
lg<-lg+theme(legend.position="none")
lg<-lg + theme(axis.title.x=element_text(size=rel(1),color=bvColors[3])) + xlab(" More Income -->")
lg<-lg + theme(axis.title.y=element_text(size=rel(1),color=bvColors[3])) + ylab("  More Seats -->")
lg<-lg+theme(axis.text=element_blank())
lg<-lg+theme(line=element_blank())
#put both plots on a grid
ggdraw()+ draw_plot(lg,0.1,0.65,width=0.2,height=0.2) + draw_plot(gg)

#########################################################################################
#crosstab of number of zip codes in income and seat Bins
xtab<-table(allData$seatsBin,allData$incomeBin)
hm<-as.data.frame(xtab)
names(hm)<-c("SeatsPer100Kids","IncomePerCapita","Freq")

#display cross-tab
print(xtab)
#show heatmap of crosstab
# this suggests that high income zip codes are underserved with pre-K seats
ggplot(hm, aes(SeatsPer100Kids, IncomePerCapita)) + geom_tile(aes(fill = Freq),colour = "white") +
       scale_fill_gradient(low = "steelblue", high = "white")

