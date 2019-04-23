binnarize <- function(data, cols_to_bin){
  # check if package is loaded ----
  if(!any(.packages() %in% "magrittr")) library(magrittr)
  if(!any(.packages() %in% "data.table")) library(data.table)

  # make sure data inherits "data.table" class ----
  if(!inherits(data, "data.table")) data <- data.table(data)
  
  res <- lapply(seq_along(cols_to_bin), function(x){
    cols <- names(data)
    sym <- as.name(cols_to_bin[[x]])
    res <- data.table::dcast(data,
                             formula=...~eval(sym),
                             fill=0,
                             fun.aggregate=function(x) 1,
                             value.var=list(cols_to_bin[[x]]))
    resCols <- setdiff(names(res), cols)
    colsLab <- paste0(cols_to_bin[[x]], "_", resCols)
    names(res)[which(names(res) %in% resCols)] <- colsLab
    removeCol <- colsLab[[1]] # remove baseline
    cat(crayon::yellow(paste0("Baseline column removed: ", removeCol, ".\n\n")))
    return(suppressWarnings(res[, -c(cols_to_bin, removeCol), with = F]))
    }) %>%
    Reduce(f = function(x, y) x[y, on=intersect(names(x), names(y)), nomatch=0L], x = .)
  
  cols <- grep(x=names(res), pattern="(?i)\\#", value=T)
  repl <- gsub(x=cols, pattern="\\#", replacement = "sharp")
  setnames(res, cols, repl)
  cols <- grep(x=names(res), pattern=" ", value=T)
  repl <- gsub(x=cols, pattern=" ", replacement = "_")
  setnames(res, cols, repl)
  
  # standardize some vars ----
  cols <- c("loudness", "tempo", "duration_ms", "time_signature") 
  res[, (cols) := lapply(.SD, function(x)(x-min(x,na.rm=T))/(max(x,na.rm = T)-min(x,na.rm = T))), .SDcols=cols]
  rm(cols)
  
  return(res)
}