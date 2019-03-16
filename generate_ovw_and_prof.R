library(readxl)
library(dplyr)

courts <- read_excel("beachvolley_courts.xlsx", sheet= 3)

# helpers #############
wrap <- function(x, tag = "td", class = NULL){
    if(is.null(class)){
      sprintf("<%s>%s</%s>", tag, x, tag)   
    } else {
      sprintf('<%s class="%s">%s</%s>', tag, class,  x, tag)   
    }
} 

wrwr <- function(x, nms){
  wrap(paste0(wrap(paste0(nms,": ")),
              wrap(x)),"tr")
  
} 

addClass <- function(s, tr_class = NULL, td_class = NULL){
  if(!is.null(tr_class)){
    s <- gsub("<tr>", sprintf('<tr class="%s">',tr_class),s)  
  }
  if(!is.null(td_class)){
    s <- gsub("<td>", sprintf('<td class="%s">',td_class),s)  
  }
  s
} 


ll <- lapply(split(courts,courts$Name),wrwr,nms = names(courts))

