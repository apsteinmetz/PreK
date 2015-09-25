# test bed to get and parse PDF's directly from web

library(acs)
library(stringr)
library(dplyr)
library(tm)
library(Hmisc)
library(reshape2)


#DOE pre-k directories
urls<- c("http://schools.nyc.gov/NR/rdonlyres/1F829192-ABE8-4BE6-93B5-1A33A6CCC32E/0/2015PreKDirectoryManhattan.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/5337838E-EBE8-479A-8AB5-616C135A4B3C/0/2015PreKDirectoryBronx.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/F2D95BF9-553A-4B92-BEAA-785A2D6C0798/0/2015PreKDirectoryBrooklyn.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/B9B2080A-0121-4C73-AF4A-45CBC3E28CA3/0/2015PreKDirectoryQueens.pdf",
         "http://schools.nyc.gov/NR/rdonlyres/4DE31FBF-DA0D-4628-B709-F9A7421F7152/0/2015PreKDirectoryStatenIsland.pdf")

dests <- tempfile(str_match(urls,"Directory(\\w.+).pdf")[,2],fileext = ".pdf")

for i in 

# pdftotxt.exe is in current directory and convert pdf to text
exePath = "C:\\Users\\nsteinm\\Documents\\R\\"
exe <- paste(exePath,"pdftotext.exe",sep="")
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt)


# do something with it, i.e. a simple word cloud 
#library(tm)
#library(wordcloud)
#library(Rstem)

txt <- readLines(filetxt) # don't mind warning..




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
header<-c("District","Address","zip","Add","Note","Total")
names(pke2)<-header
pke2$Add<-as.numeric(levels(pke2$Add)[pke2$Add])
pke2$Total<-as.numeric(levels(pke2$Total)[pke2$Total])
byzip<-group_by(pke2,zip)
aggNew<-summarise(byzip,newSeats=sum(Total,na.rm=TRUE))
names(aggNew)<-c("region","value")
zip_choropleth(aggNew,
               county_zoom=nyc_fips,
               title="2014 New York City Pre-K Seats added",
               legend="New Seats")

#joint<-left_join(aggNew,tax1,by="region")
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

