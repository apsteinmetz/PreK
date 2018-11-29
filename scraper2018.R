# download and scrape NYC PRE-K pdf directories
# -----------------------------------------------------------------------
# get NYC data on pre-K programs in 2018
# scan seat directory pdfs and put into a data frame by zip code
#DOE pre-k directories

library(tidyverse)
library(pdftools)


#urls valid as of November 2018
urls<-c("https://www.schools.nyc.gov/docs/default-source/default-document-library/2018nycprekdirectorybronx-english",
        "https://www.schools.nyc.gov/docs/default-source/default-document-library/nyc-pre-kindergarten-directory-brooklyn",
        "https://www.schools.nyc.gov/docs/default-source/default-document-library/2018-nyc-prek-directory-manhattan-english",
        "https://www.schools.nyc.gov/docs/default-source/default-document-library/2018nycprekdirectoryqueens-english",
        "https://www.schools.nyc.gov/docs/default-source/default-document-library/2018nycprekdirectorystatenislandenglishweb-english")

boroughs <- c('Manhattan','Bronx','Brooklyn','Queens','Staten')

#regex to parse address line
pkseattokens <-"(Address: )([.[:alnum:]- ()]+),+ ([0-9]{5})([a-zA-Z .()-:]+) ([0-9]{1,4}) (FD|HD|AM|PM|5H)"

dests <- paste0("pdf/",boroughs,".pdf")

for (i in 1:length(urls)) {
  download.file(urls[i],destfile = dests[i],mode = "wb")
}

listings <- NULL
for (i in 1:length(dests)) {
  print(dests[i])
  txt <- suppressMessages(pdf_text(dests[i]))
  # "Playspace:" is marker for actual directory listing of programs on that page
  # Discard other pages
  txt <- txt[str_detect(txt,"Playspace:")]
  txt <- txt[-1] #first instance is an example page
  listings <- append(listings,txt)
  # file.remove(dests[i])
}

# find address line which contains zip and seat count
txt2<-txt[grep("Address:",txt)]
# strip chars that will mess up regex
pkseattokens <-"(Address: )([.[:alnum:]- ()]+),+ ([0-9]{5})([a-zA-Z .()-:]+) ([0-9]{1,4}) (FD|HD|AM|PM|5H)"
txt2<-sub("'","",txt2)
schools<-as_data_frame(str_match(txt2,pkseattokens))[,c(4,6,7)]
names(schools)<-c("zip","seats","dayLength")
#have to convert from factor to character THEN to integer.  Don't know why
schools$seats<-as.integer(as.character(schools$seats))

# aggregate seat count by zip code
sumSeats <- schools %>% 
  group_by(zip) %>% 
  summarise(count = n(), 
            numSeats = sum(seats, na.rm = TRUE))
names(sumSeats)<-c("zip","schools","numSeats")
