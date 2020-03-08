# This scripts makes sure that eplc data is uniform with the pop per gov unit data frame. This would make joining the two tables easier
 
library(data.table)
library(magrittr)
library(stringr)

eplc_dedup <- readRDS("bin/eplc_preproc.rds")
locVars <- c("implementing_office", "project_location", "project_description")
eplc_dedup[,c(locVars) := lapply(.SD, str_to_lower), .SDcols = eval(locVars)]
eplc_dedup[,implementing_office:=str_to_lower(implementing_office)]
eplc_dedup[,project_location := str_to_lower(project_location)]
eplc_dedup[,UpperLevel:=str_replace_all(implementing_office, "district engineering office", "") %>%
           str_replace_all("\\s+\\d{1}\\w+", "") %>%
           str_replace_all("\\s+$", "")]

#### EDGE CASES #####
# fix ñ characters
eplc_dedup[str_detect(project_location , "\\\x96"),
           project_location := str_replace_all(project_location, "\\\x96", "ñ")]
# changing some location strings to more relevant ones
eplc_dedup[UpperLevel == "mt.province", UpperLevel := "mountain province"]

# change dinagat island to dinagat islands
eplc_dedup[UpperLevel == "dinagat island", UpperLevel := "dinagat islands"]

# remove punctuations in region iv upper level
eplc_dedup[str_detect(UpperLevel, "region iv"),
           UpperLevel := str_replace_all(UpperLevel,"-", "")]

# change southern mindoro to oriental mindoro
eplc_dedup[UpperLevel == "southern mindoro", UpperLevel := "mindoro oriental"]

# remove punctations in project_location
eplc_dedup[,project_location := str_replace_all(project_location,"\\(.*\\)", " ") %>%
             str_replace_all("^\\s+|\\s+$", "") %>%
             str_replace("-", " ") %>%
             str_replace("[[:punct:]]", "") %>% 
             str_replace("\\s{2,}", "\\s")]

saveRDS(eplc_dedup, "bin/eplc-loc-proc.rds")