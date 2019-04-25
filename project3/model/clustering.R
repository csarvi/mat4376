# pkg requirements ----
source("helpers/checkPkg.R")
pkgs <- c("magrittr", "data.table", "diceR")
for(i in seq_along(pkgs)){
  checkPkg(pkg=pkgs[[i]])
}

# load packages (if ncessary) ----
pkgs <- c("magrittr", "data.table")
for(i in seq_along(pkgs)){
  if(!any(.packages() %in% pkgs[[i]])) library(pkgs[[i]], character.only = T)
}
rm(pkgs, i)

# load data ----
data <- readRDS("data/data.RDS")

# make it into matrix with features - use track names as row id
m <- as.matrix(data[, -c("track_name", "artist_name", "user_name")])
rownames(m) <- data[, track_name]


# consensus clustering ----
CC <- diceR::consensus_cluster(data = m, nk=3:10, p.item=.8, 
                               rep=10, 
                               algorithms = c("km", "hc", "ap", "sc"), 
                               distance = "euclidean", # valid for "km", "hc" 
                               seed.data = 1234)

# consensus summaries
ccomb_matrix <- diceR::consensus_combine(CC, element="matrix")
ccomb_class <- diceR::consensus_combine(CC, element="class")
ccomp <- diceR::consensus_evaluate(data=m, CC, plot=FALSE)

# bundle results in a list and save it
out <- list(CC, ccomb_matrix, ccomb_class, ccomp)
names(out) <- c("ensemble", "consensus_matrix", "consensus_class", "consensus_evaluate")
saveRDS(out, "model/clustering_res.RDS")