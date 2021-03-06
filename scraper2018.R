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

dests <- paste0("pdf/",boroughs,"2018_Pre_K.pdf")

# Download PDF directories from NYC if PDFs are not already present
if (!is.na(match(FALSE,file.exists(dests)))) {
  for (i in 1:length(urls)) {
    download.file(urls[i],destfile = dests[i],mode = "wb")
  }
}
# extract and combine text from PDFs
listings <- NULL
for (i in 1:length(dests)) {
  print(dests[i])
  txt <- suppressMessages(pdf_text(dests[i]))
  # "Playspace:" is marker for actual directory listing of pre-k schools on that page
  # Discard other pages
  txt <- txt[str_detect(txt,"Playspace:")]
  txt <- txt[-1] #first instance is an example page. discard.
  listings <- append(listings,txt)
  # file.remove(dests[i])
}

#text is in one page per item
#divide listings into separate lines
listings <- listings %>% str_split("\\r?\\n") %>% unlist()

# regex to extract valid address
#addresstokens <-"(Address: )['-./0-9 A-Za-z]+,[0-9 A-Za-z]+([0-9]{5})"
addresstokens <-"(Address: ).+([0-9]{5})"
#use lines containing address as anchor to mark record boundaries
lines_address<-grep(addresstokens,listings)
record_start<-lines_address-1
record_end<-(lines_address-2)[-1] %>% append(length(listings))

#workhorse function to create a record with all relevant info about a school
create_record <- function(rec_index){
  #rec <- listings[record_boundaries[rec_index,]$start:record_boundaries[rec_index,]$end]
  rec <- listings[record_start[rec_index]:record_end[rec_index]]
  name <- str_extract(rec[1],"(.+)(?=\\|)") %>% str_trim()
  address <- str_extract(rec[2],addresstokens)%>% str_remove("Address: ")
  zip <- str_extract(address,"[0-9]{5}$")
  address <- str_remove(address," [0-9]{5}$")
  borough <- address %>% str_extract("[A-z]+(?= NY)")
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
schools_2018 <- 1:length(record_start) %>% map(create_record) %>% bind_rows()
save(schools_2018,file="data/schools_2018.rdata")


# aggregate seat count by zip code
sumSeats <- schools_2018 %>% 
  group_by(zip) %>% 
  summarise(count = n(), 
            numSeats = sum(seats, na.rm = TRUE))
names(sumSeats)<-c("zip","schools","numSeats")
# aggregate seat count by zip code

sumSeats <- schools_2018 %>% 
  group_by(borough) %>% 
  summarise(count = n(), 
            numSeats = sum(seats, na.rm = TRUE))
names(sumSeats)<-c("borough","schools","numSeats")
sumSeats
