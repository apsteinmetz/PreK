#test hand rolled image overlay for use with MapView package.
library(dplyr)
library(leaflet)

# convert a local image file to raw base64 and embed in HTML.

# create HTML embedding a raw image
img_html <- function(img,height=100,width=100,alpha = 1){
  # style has no effect. Why?
  style <- paste0('style="opacity:',
                  alpha,
                  ';filter:alpha(opacity=',
                  alpha * 100, ')"')
  #convert an image file to base 64 to embed raw data in HTML
  img_raw <- knitr::image_uri(img)
  legend_html<- paste0('<img src="',
                       img_raw,'"',
                       ', width=', width,',',
                       'height=', height,',',
                       style,'>') %>% 
    htmltools::HTML()
}
my_addLogo <- function(map,img,position="topleft",alpha=1,height=100,width=100){
  m <- map@map
  m <- m %>%  
    addControl(img_html("img/bv_legend.png",height=height,width=height,alpha=alpha),
               position="topleft")
  map@map <- m
  return (map)
}


#test
mv %>% my_addLogo(img = "img/bv_legend.png",width = 100,height=100)


