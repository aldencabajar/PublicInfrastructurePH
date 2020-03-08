library(data.table)
library(stringr)
library(magrittr)

# load needed tables for the use of these functions
eplcLocProc <- readRDS("bin/eplc-loc-proc.rds")
pop_proc <- readRDS("bin/PopnPerGovUnit-proc.rds")

### TEST PARS ####

# data <- eplcLocProc
# project_loc <- "project_location"
# project_desc <- "project_description"
# UpperLevel <- "UpperLevel"
# pop_df <- pop_proc


FindLoc = function(UpperLevel, project_loc, project_desc, pop_df = NULL, data = NULL) {
"The method goes like this:
+ search the corresponding region/city/province in the `implementing_office` column
+ From the found region/city/province, narrow the search and find the corresponding city if a province was found,
province if a region was found, and barangay if a city was found. This was then matched with the `project_location` column.
+ Finally if no location was found across the hierarchy of government units, the method will retun a null value.
 
Args: 
  UpperLevel = perceived upper government unit as derived from the district engineering office 
  project_loc = the detailed project location
  project_desc = Description of the project
  pop_df = data frame containing fields: Region, City_Municipality, Barangay, District, Location, Population 
  data = eplc data containing the fields as defined in UpperLevel, project_loc, and project_desc
  
  "
 
  project_location <- data[[project_loc]]
  project_description <- data[[project_desc]]
  UpperLevel <- data[[UpperLevel]]
  
  # determine the location from project_location
  projectLocUnique <- unique(project_location)
  loc <- pop_df$location
  
  # preliminary finding of possible candidates using regular expressions
  FromProjLoc <-
    lapply(projectLocUnique, function(x)
      str_which(loc, paste0("\\b", x, "\\b")))
  FromProjLocDf <- data.table(project_location = projectLocUnique,
                              FromProjLoc = FromProjLoc)
  df <-
    data.table(
      UpperLevel = UpperLevel,
      project_location = project_location,
      project_description = project_description
    )
  # assign pseudo ids to each entry for use later when joining
  df[,id := 1:.N]
 
  # associating the possible candidates to a project location
  df[FromProjLocDf, Cnd := i.FromProjLoc, on = "project_location"]
  
  ### Main process of assigning government units and population to a project location
   df[, BestLocation := list(mapply(
     function(x, y) DetectBestLoc(x, y , pop_df),
     Cnd,
     UpperLevel,
     SIMPLIFY = F
     ))]
   bestLoc <- df$BestLocation
   
   df[,BestLocation := bestLoc]
   df[cbind(df[,.(id)], rbindlist(df$BestLocation)), 
      ':='(
        Region = i.Region,
        Province = i.Province,
        City_Municipality = i.City_Municpality,
        Barangay = i.Barangay,
        population = i.population
      ), on = "id"]
   df[,BestLocation := NULL]
   
   return(df)
   
}

 
DetectBestLoc = function(Cnd, UpperLevel, pop_df)  {
   " Using candidate indices, find the most appropriate location by matching with the relevant Province, Region or
   # City/Municipality in the implementing office section"
   
   Cnd <- unlist(Cnd)
   
   if (length(Cnd) == 0) {
     # if length of candidates is 0, go directly to searching UpperLevel
     
     RelevantData <-
       pop_df[str_detect(location, paste0("\\b", UpperLevel, "\\b")) &
                level %in% c("City_Municipality", "Province", "Region"),
              .(Province, Region, City_Municipality, location, population)]
     
     # if regular expressions give multiple matches, try doing an exact match
     if (nrow(RelevantData) > 1) {
       RelevantData <- RelevantData[location == UpperLevel]
     }
     
     Loc <- RelevantData[, -5]
     BestMatchIndex <- 1
     
   }
   
   if (length(Cnd) > 0)  {
     RelevantData <-
       pop_df[Cnd,] %>%
       .[, .(Region, Province, City_Municipality, location, population)]
     
     if (length(Cnd) == 1) {
       # if Cnd is equal to 1, it is most probable that it has only one match in the population df, and it is assumed
       # that it is reasonable to directly query its corresponding tuple (Province, Region, City_Municipality, Barangay)
       Loc <- RelevantData[, 1:4] 
       BestMatchIndex <- 1
       
     } else {
       BestMatchIndex <-
         lapply(RelevantData[, .(Province, Region, City_Municipality)],
                function(x)
                  str_detect(x, UpperLevel)) %>%
         as.data.table() %>%
         .[, sum_bool := apply(.SD, 1, sum, na.rm = T)] %>%
         .[["sum_bool"]] %>%
         which.max()
       
       Loc <- RelevantData[BestMatchIndex, 1:4]
       
     }
   }
   
   # for edge cases where two locations are indistinguisable such as
   #siquijor town and siquijor province, prefer province level hence:
   if (nrow(Loc) > 1) {
     Loc <- Loc[is.na(Province)]
   }
   
   # if no candidate was found, return NULL values for each of the fields in the tuple
   if (nrow(Loc) == 0) {
     return(
       data.table(
         Region = NA,
         Province = NA,
         City_Municipality = NA,
         Barangay = NA,
         population = NA
       )
     )
   }
   #identify null columns for restructuring the necessary information
   NACols <- which(unname(sapply(Loc, is.na, USE.NAMES =  F)))
   
   # removing null columns and relabeling the columns by hierarchy
   if (length(NACols) != 0) {
     Loc[, (NACols) := NULL]
     Loc[, (paste0("V", 1:length(NACols))) := NA]
     setnames(Loc,
              colnames(Loc),
              c("Region", "Province",
                "City_Municipality", "Barangay"))
   }
   
   # column bind the population col at index 5
   return(cbind(Loc, RelevantData[BestMatchIndex, 5]))
}

findLocObj <-
  FindLoc(
    "UpperLevel",
    "project_location",
    "project_description",
    pop_df = pop_proc,
    data = eplcLocProc
  )

saveRDS(findLocObj, "bin/eplc-w-pop.rds")