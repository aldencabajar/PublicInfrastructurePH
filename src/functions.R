#dt <- readd(dt2014)
# dt2014 <- readd(dt2014)

munge_func = function(dt){
dt %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, funs(if_else((grep(" *", ., useBytes = T) == T)|. == "", NA_character_, .))) %>%
    filter(!is.na(project_cost)) %>%
  # filter_at(vars(matches(("project_cost"))), funs(!is.na(.)))
    mutate(actual_strt_dte = sprintf("%i-%i", actual_start_year, actual_start_month),
           actual_cmpl_dte = sprintf("%i-%i", actual_completion_year, actual_completion_month)) %>%
    mutate_at(vars(matches("_dte")), as.yearmon) %>%
    mutate(constr_time = (actual_cmpl_dte - actual_strt_dte)*12) #%>%
  # View

  
}

str_munge = function(dt, doc_col, ..., ngram = c(1L, 1L)){
  require(text2vec)

  doc_col <- enquo(doc_col)
  
  prep = function(x) {
    x %>% 
      str_to_lower %>%
      str_replace_all("[^[:alpha:]]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_remove_all(" .{1} ")
  }
  
  doc <- pull(dt, !!doc_col) %>% prep
  it <- itoken(doc, progressbar = F)
  vocab <- create_vocabulary(it, ngram = ngram, stopwords = tm::stopwords()) %>%
                prune_vocabulary(...)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  
  return(list(vocab = vocab, dtm = dtm))
}



