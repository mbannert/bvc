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


courts %>% 
  filter(Name == a) %>% 
  select(latitude, longitude)




replace_table <- function(tr_element, temp){
  gsub("####_TABLE_####",tr_element,temp)
}

replace_coords <- function(fn, file, df){
  element <- df %>% 
    filter(file_name == fn) %>% 
    select(longitude, latitude)
  
  file <- gsub("####_LONG_####", element$longitude, file)
  gsub("####_LAT_####", element$latitude, file)
}


# call functions, generate court profiles :)
# read in a template file in order to replace the placeholders :)
template <- paste(readLines("bvc/courts/court_template.html"),
                  collapse = "\n")
ll <- lapply(split(courts,courts$Name),wrwr,nms = names(courts))
ll_collapsed <- lapply(ll,function(x) paste(x,collapse="\n"))
html_files <- lapply(ll_collapsed, replace_table, temp = template)

# replace umlauts and other ugly chars 
# to be able to generate nice file names
names(html_files) <- tolower(gsub(" |\\.\\'","_",names(html_files)))
names(html_files) <- gsub("ö","oe",names(html_files))
names(html_files) <- gsub("ä","ae",names(html_files))
names(html_files) <- gsub("ü","ue",names(html_files))

# add file name to initial tibble 
courts$file_name <- names(html_files)

html_files <- lapply(names(html_files), function(x){
  replace_coords(x, html_files[[x]], courts)
})

names(html_files) <- courts$file_name


lapply(names(html_files), function(x) {
  cat(html_files[[x]],file = sprintf("bvc/courts/%s.html",x))  
})


# generate an overview table
courts$link <- sprintf("courts/%s.html", courts$file_name)


overview <- courts %>% 
  select(id, Name, link, Courts, Kategorie, longitude, latitude)

display <- overview %>% 
  select(Name, Courts, Kategorie)

xx <- knitr::kable(display,"html")


cat(xx,file = "test.html")


