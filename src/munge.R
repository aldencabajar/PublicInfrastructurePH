library(data.table)
library(zoo)
library(dplyr)
library(readr)

eplc2014 <- read_csv("eplcproj2014.csv", local = locale(encoding = "latin1"))
eplc2015 <- read_csv("eplcproj2015.csv", local = locale(encoding = "latin1"))

eplc_dt <- bind_rows(eplc2014, eplc2015)
#eplc_dt <- sample_n(eplc_dt, 100)
dt <- eplc_dt %>%
  mutate(strtdte_yearmo = sprintf("%i-%i", actual_start_year, actual_start_month),
         cmpldte_yearmo = sprintf("%i-%i", actual_completion_year, actual_completion_month)) %>%
  ungroup()%>%
  mutate_at(vars(matches("yearmo")), as.yearmon) %>%
  mutate(mo_cnst = (cmpldte_yearmo - strtdte_yearmo)*12)

yearmo_vars <- grep("yearmo", colnames(eplc_dt), value = T)

microbenchmark(
  
dt_1 <- eplc_dt %>%
    mutate(strtdte_yearmo = sprintf("%i-%i", actual_start_year, actual_start_month),
           cmpldte_yearmo = sprintf("%i-%i", actual_completion_year, actual_completion_month)) %>%
    ungroup()%>%
    mutate_at(vars(matches("yearmo")), as.yearmon) %>%
    mutate(mo_cnst = (cmpldte_yearmo - strtdte_yearmo)*12),
  

dt_2 <- setDT(eplc_dt)[, ':='(strtdte_yearmo =  sprintf("%i-%i", actual_start_year, actual_start_month),
               cmpldte_yearmo = sprintf("%i-%i", actual_completion_year, actual_completion_month))
               ][
                 ,rlang::eval_tidy(yearmo_vars) := lapply(.SD, as.yearmon),
                 .SDcols = yearmo_vars
               ][
                 ,mo_cnst := (cmpldte_yearmo - strtdte_yearmo)*12
               ]

)

project_desc <- gsub('[[:punct:]]|\\\\', " ", dt$project_description)
crp <- VCorpus(VectorSource(project_desc))

crp <- tm_map(crp, stripWhitespace)
crp <- tm_map(crp, content_transformer(tolower))
crp <- tm_map(crp, removeWords, stopwords("en"))
crp <- tm_map(crp, removeNumbers)

dtm <- DocumentTermMatrix(crp, control = list(wordLengths = c(4, Inf)))
dtm_fnl <- removeSparseTerms(dtm, 0.95)

dt <- bind_rows(dt, as_tibble(as.matrix(dtm)))
# inspect(dtm_fnl)
# inspect(dtm)

saveRDS(dt, "munge-dt.RDS")



# gsub("District|Engineering|[1-9][tsnr][hdt]|Office", "",  dt$implementing_office) %>% tolower() %>% unique()
# 
# map(str_split(dt$project_description, pattern = "[[:space:]]|[[:punct:]]"), ~.[. != ""])
