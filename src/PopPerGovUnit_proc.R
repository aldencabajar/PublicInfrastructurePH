#Using some patterns that were identified during the exploration, adjust some location strings to keep it uniform
#with the eplc data project locations

library(data.table)
library(stringr)
library(magrittr)

PopnPerGovUnit <- readRDS("bin/Population_per_gov_unit.RDS")

#Transform string variables into lower case
StringVars <- c("Region", "Province", "City_Municipality", "location")
PopnPerGovUnit[,c(StringVars):=lapply(.SD, str_to_lower),.SDcols = eval(StringVars)]
PopnPerGovUnit[,locOriginal:= location]

# Remove words inside parenthesis
PopnPerGovUnit[,location := str_replace_all(location, "\\(.*\\)", "")]

# remove numbers in City and municipal names
PopnPerGovUnit[level == "City_Municipality", location := str_replace_all(location, "\\d|\\d\\s$", "")]

# remove punctuation marks
PopnPerGovUnit[,location := str_replace_all(location, "[[:punct:]]", "")]
PopnPerGovUnit[,location := str_replace_all(location, "^\\s|\\s+$", "") %>% 
                 #remove more than one white space
                 str_replace_all("\\s{2,}", " ")]

### EDGE CASES ####
# changing some instances of "city of" into just "<name> city, retain the island garden city of Samal and Science city of Muñoz as these are legitimate names"
PopnPerGovUnit[!str_detect(location,"island garden|science city"), 
               location:= str_replace_all(location, "city of (.*)$", "\\1 city")] 
# Change caraga to region XIII
PopnPerGovUnit[location == "caraga" & is.na(Region), location := "region xiii"]
# interchange occidental and oriental in mindoro i.e. instead of occidental mindoro, mindor occidental 
PopnPerGovUnit[str_detect(location, "[occidental, oriental] mindoro"),
               location := str_replace_all(location, "(.+) mindoro", "mindoro \\1")]

# Remove provinces in the national capital region 
PopnPerGovUnit[Region == "national capital region", Province := NA]

# add a District column to accomodate the lack of Provinces in the national capital region
PopnPerGovUnit[City_Municipality %in% c("tondo", "binondo", "santa cruz", "san nicolas",
                                        "ermita", "malate", "pandacan", "santa ana",
                                        "san miguel", "intramuros", "paco", "port area",
                                        "quiapo") & Region =="national capital region",
               City_Municipality := "manila"] 



# A special is case is Manila, where it is subdivided into corresponding legislative districts. But in this data frame, Manila is subdivided into its geographic districts (Tondo, Binondo, Sta. Cruz etc.) 
# district 1 brgy 1-146
PopnPerGovUnit[City_Municipality == "manila" & 
                 (as.integer(unlist(str_extract(location, "\\d+"))) %in% 1:146) &
                 level == "barangay", District := "manila district 1"]
# district 2 147-267
PopnPerGovUnit[City_Municipality == "manila" & 
                 (as.integer(unlist(str_extract(location, "\\d+"))) %in% 147:267) &
                 level == "barangay", District := "manila district 2"]
# district 3 268-394
PopnPerGovUnit[City_Municipality == "manila" & 
                 (as.integer(unlist(str_extract(location, "\\d+"))) %in% 268:394) &
                 level == "barangay", District := "manila district 3"]
# district 4 395-586
PopnPerGovUnit[City_Municipality == "manila" & 
                 (as.integer(unlist(str_extract(location, "\\d+"))) %in% 395:586) &
                 level == "barangay", District := "manila district 4"]
# district 5 649-828
PopnPerGovUnit[City_Municipality == "manila" & 
                 (as.integer(unlist(str_extract(location, "\\d+"))) %in% 649:828) &
                 level == "barangay", District := "manila district 5"]
# district 6 587-648, 829-905
PopnPerGovUnit[City_Municipality == "manila" & 
                 (as.integer(unlist(str_extract(location, "\\d+"))) %in% c(587:648, 829:905)) &
                 level == "barangay", District := "manila district 6"]

# save processed population per gov unit df 
saveRDS(PopnPerGovUnit, "bin/PopnPerGovUnit-proc.rds")