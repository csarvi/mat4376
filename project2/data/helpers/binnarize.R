binnarize <- function(data, cols_to_bin){
  # package installation check ----
  if(!nzchar(system.file(package = "magrittr"))){
    ans <- menu(choices = c("Y", "N"), 
                title = "Package magrittr not installed in your system.\n\nDo you wish to install it? (The function will thrown an error if 'N')")
    if(ans == 2L) stop("Execution aborted.")
    install.packages("magrittr")
  }
  if(!nzchar(system.file(package = "data.table"))){
    ans <- menu(c("Y", "N"), title = "Package data.table not installed in your system.\n\nDo you wish to install it? (The function will thrown an error if \"N\".)")
    if(ans == 2L) stop("Execution aborted.")
    install.packages("data.table")
  }
  if(!nzchar(system.file(package = "crayon"))){
    ans <- menu(choices = c("Y", "N"), 
                title = "Package crayon not installed in your system.\n\nDo you wish to install it? (The function will thrown an error if 'N')")
    if(ans == 2L) stop("Execution aborted.")
    install.packages("crayon")
  }
  
  # check if package is loaded ----
  if(!any(.packages() %in% "magrittr")) library(magrittr)
  if(!any(.packages() %in% "data.table")) library(data.table)
  
  
  # make sure data inherits "data.table" class ----
  if(!inherits(data, "data.table")) data <- data.table(data)
  
  lapply(seq_along(cols_to_bin), function(x){
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
    Reduce(f = function(x, y) x[y, on=intersect(names(x), names(y)), nomatch=0L], x = .) %>%
    return(.)
}