getScore <- function(mod, dfmMod, txt){
  # insert text in a dataframe
  txt <- data.frame(text= txt, 
                    stringsAsFactors = FALSE)
  
  # unnest tokens
  txt2 <- tidytext::unnest_tokens(txt, word, text)
  
  # remove stopwords
  txt2 <- txt2[which(!(txt2$word %in% stopwords::stopwords("en", "stopwords-iso"))), , drop=FALSE]
  # remove digits
  txt2 <- txt2[!grepl(x=txt2$word, pattern = "\\d"), , drop=FALSE]
  # count remaining words in txt
  txt2 <- data.frame(table(txt2$word), stringsAsFactors = FALSE)
  # rename cols
  names(txt2) <- c("word", "n")
  # create fake ID for dfm creation
  txt2$ID <- 1
  
  # create dfm matrix
  dfmNew <- tidytext::cast_dfm(data = txt2, document = ID, term = word, value = n)
  
  # intersect with features in the model's document feature matrix
  dfmNew <- quanteda::dfm_select(dfmNew, dfmMod, valuetype="fixed")
  
  # get feature intersection ----
  res <- predict(mod, dfmNew, type="response")
  
  return(res)
}