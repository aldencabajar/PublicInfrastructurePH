# This script serves to extract population data from the xls files download from psa.gov.ph


library(tidyxl)
library(data.table)
library(magrittr)

xlfilePath <- xl_files[6]

ExtractPopulation = function(xlfilePath) {
  
  # Read xls files from dir
  frmt <- xlsx_formats(xlfilePath)
  reg <- xlsx_cells(xlfilePath, include_blank_cells = F)
  
  # extract the character and corresponding populations  
  pop <- reg[reg$local_format_id %in% which(frmt$local$font$bold), 
             c('character', 'numeric', 'row')]
  setDT(pop)
  
 
  #using the same data table join the corresonding population using the row number.
  # this is possible since the place name is aligned with its population number
  pop[pop[,.(row, numeric)], population := i.numeric, on = 'row']
  pop_new <- na.omit(pop, cols = c("character", "population"))[,-c("numeric")]
  
 # Detecting provinces. We can detect it using the fact that it is immediately succeeded by a character
 pop_new[,':='(
   lead_row = shift(row, n = 1, type = "lead"),
   lag_row = shift(row, n = 1, type = "lag")
 )] 
 
 # provinces have differences between its row and succeeding row of not more than 3
 pop_new[(lead_row - row) <= 2 & !is.na(lag_row),level := "Province"]
  
  
  pop_new[1, level := 'Region']
  pop_new[is.na(level), level:='City_Municipality']
  pop_new[,':='(lead_row = NULL, lag_row = NULL)]
 
  #Determine barangay population 
  ## include only entries that are not in bold. These are the barangay entries
  pop_brgy <- reg[!(reg$local_format_id %in% which(frmt$local$font$bold)),
                    c('character', 'numeric', 'row')]
  setDT(pop_brgy)
  pop_brgy[pop_brgy[,.(row, numeric)], population := i.numeric, on = 'row']
  pop_brgy_new <- na.omit(pop_brgy, cols = c("character", "population"))[,-c("numeric")]
  pop_brgy_new[,level:='barangay']
  
  setkey(pop_new, row)
  setkey(pop_brgy_new, row)
  
  BindPop <- rbind(pop_new, pop_brgy_new)[order(row)] 
  BindPop[,level := factor(level, levels = c("barangay", "City_Municipality", "Province", "Region"))]
  BindPop[,level_int:=as.integer(level)]
  setkey(BindPop , row)
 split_list <- split(BindPop[level_int > 1], by = "level", drop = T) 
 split_list[[length(split_list) +1]] <- BindPop
  
  
  BuildHierarchy = function(pop_df, df) {
        
  levelName <- as.character(unique(pop_df$level))
    
  pop_df %>%
    .[,.(character, row , level_int)] %>%
    setnames("character", levelName) %>%
    setkey(row) %>%
    .[
      df,
       roll = T
    ]  %>%
    .[level_int <= i.level_int, eval(levelName) := NA] %>%
    .[,level_int := NULL] %>%
    setnames("i.level_int", "level_int") %>%
    .[]
    
  }
  
  Reduce(BuildHierarchy, split_list, right = T) %>%
    .[,':=' (row = NULL, level_int = NULL)] %>%
    setnames("character", "location") %>%
    .[]
  
}


xl_files <- paste('region-pop/',list.files('region-pop/'), sep = '')
PopList <- lapply(xl_files, ExtractPopulation)




PopListBind <- rbindlist(PopList)

saveRDS(PopListBind,'Population_per_gov_unit.RDS')
