checkPkg <- function(pkg=NULL){
  if(is.null(pkg)) stop("\"pkg\" parameter is NULL.")
  if(length(pkg) > 1) {
    warning("\"pkg\" length > 1. Only the first element is selected.")
    pkg <- pkg[[1]]
  }
  
  if(!nzchar(system.file(package = pkg))){
    ans <- menu(choices = c("Y", "N"), 
                title = pasteo("Package ", pkg, " not installed in your system.\n\nDo you wish to install it? (The function will thrown an error if 'N')"))
    if(ans == 2L) stop("Execution aborted.")
    install.packages(pkg)
  } 
}

