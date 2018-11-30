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

boroughs <- c('Bronx','Brooklyn','Manhattan','Queens','Staten')

#regex to parse address line
#pkseattokens <-"(Address: )([.[:alnum:]- ()]+),+ ([0-9]{5})([a-zA-Z .()-:]+) ([0-9]{1,4}) (FD|HD|AM|PM|5H)"

dests <- paste0("pdf/",boroughs,"2018_Pre_K.pdf")

# Download PDF directories from NYC
for (i in 1:length(urls)) {
  download.file(urls[i],destfile = dests[i],mode = "wb")
}

# extract and combine text
listings <- NULL
for (i in 1:length(dests)) {
  print(dests[i])
  txt <- suppressMessages(pdf_text(dests[i]))
  # "Playspace:" is marker for actual directory listing of programs on that page
  # Discard other pages
  txt <- txt[str_detect(txt,"Playspace:")]
  txt <- txt[-1] #first instance is an example page. discard.
  listings <- append(listings,txt)
  # file.remove(dests[i])
}
#divide listings into separate lines
listings <- listings %>% str_split("\\r?\\n") %>% unlist()

# regex to extract valid address
addresstokens <-"(Address: )[0-9 A-Za-z]+,[0-9 A-Za-z]+([0-9]{5})"
#use lines containing address as anchor to mark record boundaries
lines_address<-grep(addresstokens,listings)
record_start<-lines_address-1
record_end<-(lines_address-2)[-1] %>% append(length(listings))
record_boundaries <- data_frame(start=record_start,end=record_end)

#workhorse function to create a record with all relevant info about a school
create_record <- function(rec_index){
  rec <- listings[record_boundaries[rec_index,]$start:record_boundaries[rec_index,]$end]
  name <- str_extract(rec[1],"(.+)(?=\\|)") %>% str_trim()
  address <- str_extract(rec[2],addresstokens)%>% str_remove("Address: ")
  zip <- str_extract(address,"[0-9]{5}$")
  address <- str_remove(address," [0-9]{5}$")
  borough <- address %>% str_extract(", ([A-Za-z])+ ") %>% str_remove_all("[, ]")
  # the next two searches should be tolerant of not knowing line the field is on
  day_length <- str_extract(rec,"([A-Za-z]+)(?=-Day)")  %>% na.omit() %>% as.character() %>% .[1]
  seats = str_extract(rec,"(?<=Seats: )[0-9]+") %>% na.omit() %>% as.integer() %>% .[1]
  return(data_frame(name=name,
                    address=address,
                    zip=zip,
                    borough = borough,
                    seats = seats,
                    day_length=day_length))
  
}

#create data frame with a line for each school
schools <- 1:nrow(record_boundaries) %>% map(create_record) %>% bind_rows()

# aggregate seat count by zip code
sumSeats <- schools %>% 
  group_by(zip) %>% 
  summarise(count = n(), 
            numSeats = sum(seats, na.rm = TRUE))
names(sumSeats)<-c("zip","schools","numSeats")
# aggregate seat count by zip code

sumSeats <- schools %>% 
  group_by(borough) %>% 
  summarise(count = n(), 
            numSeats = sum(seats, na.rm = TRUE))
names(sumSeats)<-c("borough","schools","numSeats")
sumSeats
