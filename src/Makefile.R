library(drake)
library(tidyverse)
library(rmarkdown)
library(zoo)
library(text2vec)

source("functions.R")


preProcess <- drake_plan(dt2014       = read.csv(file_in("https://www.dropbox.com/s/kiqdtnaq4lmycf3/eplcprojectprofile20141220.),
                   dt2015       = read.csv(file_in("https://www.dropbox.com/s/hgeg68gx4rkwwsn/eplcprojectprofile20150131.csv?raw=1")),
                   munge2014    = munge_func(dt2014),
                   project_desc = str_munge(munge2014, project_description, ngram = c(1L, 3L),
                                            doc_proportion_min = 0.001, term_count_min = 50),
                   vocab_df     = project_desc %>%
                     .$vocab %>%
                     as_tibble() %>%
                     # this line of code counts the number of _, hence the number of words being investigated
                     mutate(word_count_in_doc = str_count(term, "_") + 1)
                   )


report <- drake_plan(
  explore = rmarkdown::render(knitr_in('explore.Rmd'), output_format = 'html_document', output_dir = '.')
)
                  
