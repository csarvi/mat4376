# pkgs requirements
pkgs <- c("data.table")
for(i in seq_along(pkgs)){
  if(!any(.packages() == pkgs[[i]])) suppressWarnings(
    suppressPackageStartupMessages(library(pkgs[[i]], character.only = T))
  )
}

# process the train dataset ----

# read positive, only those with 10, 9 or 8 stars ----
filesPos <- lapply(c("/train/pos/", "/test/pos/"), 
       function(x){
         path <- sprintf("model/aclImdb_v1.tar/aclImdb_v1/aclImdb%s", x)
         dir(path = path, pattern = "[8|9|10]\\.txt", full.names = TRUE)
       })
filesPos <- unlist(filesPos)

filesNeg <- lapply(c("/train/neg/", "/test/neg/"),
                   function(x){
                     path <- sprintf("model/aclImdb_v1.tar/aclImdb_v1/aclImdb%s", x)
                     dir(path = path, pattern = "[0|1|2|3]\\.txt", full.names = TRUE)
                   })
filesNeg <- unlist(filesNeg)

# build data.frame ----
data <- matrix(nrow=sum(length(filesPos), length(filesNeg)), ncol=2)

# combine negative and positive files
files <- c(filesPos, filesNeg)
# extract sentiments
sentiments <- gsub(
  x=regmatches(files, regexpr(text=files, pattern = "\\d+\\.txt")),
  pattern = "\\.txt", replacement = "")
sentiments <- ifelse(sentiments %in% c("8", "9", "10"), 1, 0)

# read all files in a loop and insert texts
pb <- txtProgressBar(min = 0, max=length(files), initial = 0, title = "Reading text files")
for(i in seq_along(files)){
  data[i, 1] <- trimws(readLines(files[[i]], warn = F))
  setTxtProgressBar(pb, i)
}
close(pb)
data[, 2] <- sentiments

# make the matrix as data frame
out <- data.table::as.data.table(data)
setnames(out, names(out), c("text", "sentiment"))

saveRDS(out, file = "model/processed_data.RDS")
