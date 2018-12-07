#test hand rolled image overlay
library(tidyverse)
library(htmlwidgets)
library(leaflet)

### local image
my_localImage_html <- function(img, alpha=0.8, url, width=100, height=100) {
  nm <- basename(img)
  drs <- file.path(tempdir(), "graphs")
  if (!dir.exists(drs)) dir.create(drs)
  fls <- file.path(drs, nm)
  invisible(file.copy(img, file.path(drs, nm)))
  rel_path <- paste0('"', file.path("..", basename(drs), basename(img)), '"')
  
  style <- paste0(', style="opacity:',
                  alpha,
                  ';filter:alpha(opacity=',
                  alpha * 100, ');"')
  
  if (missing(url)) {
    div_html <- paste0("<img src=", rel_path,
                        ", width=", width, ", height=", height, style,
                       ", ></a>")
  } else {
    div_html <- paste0("<a href=", url, "><img src=", rel_path,
                       ", width=", width, ", height=", height, style,
                       "></a>")
  }
  
  return(div_html)
}

my_addLogo <- function(map,img,position="topleft",alpha=1,height=100,width=100){
  html <- my_localImage_html(img=img,alpha,height,width)
   methods::slot(map, "map") %>% addControl(html,position)
  
}

#test
mv %>% my_addLogo(img = "img/bv_legend.png",width = 100,height=100)

