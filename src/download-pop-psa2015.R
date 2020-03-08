#Scrape 2015 population data from PSA


library(rvest)

url <- 'https://psa.gov.ph/content/highlights-philippine-population-2015-census-population' 

# read html and determine sections with links referring to xls files
webpage <- read_html(url)
link <- webpage %>%
  html_nodes('a')  %>%
  html_attr('href') %>%
  grep('.xlsx', ., value = T)

# after getting the names of the xls files, append name to the url to get the
# complete destination.
dest <- stringr::str_replace(link, '^.*/(.*.xlsx)', '\\1')

# iterate over destination links, save in region-pop dir
for (i in 1:length(link)) {
  
  dest_dir <- paste('region-pop', dest, sep = '/')
  download.file(link[i], dest[i])
}
