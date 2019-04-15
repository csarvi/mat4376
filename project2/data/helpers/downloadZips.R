downloadZips <- function(year){
  if(!nzchar(system.file(package = "xml2"))) {
    ans <- menu(choices = c("Y", "N"), title = "The package \"xml2\" is required to run this application.\n\nDo you wish to install the package? (if \"No\", program execution stops if \"No\")")  
    if(ans == 2) stop("package will not be installed")
    install.packages("xml2")
    rm(ans)
  }
  if(!nzchar(system.file(package = "rvest"))) {
    ans <- menu(choices = c("Y", "N"), title = "The package \"rvest\" is required to run this application.\n\nDo you wish to install the package? (if \"No\", program execution stops if \"No\")")  
    if(ans == 2) stop("package will not be installed")
    install.packages("rvest")
    rm(ans)
  }
  if(!nzchar(system.file(package = "magrittr"))) {
    ans <- menu(choices = c("Y", "N"), title = "The package \"magrittr\" is required to run this application.\n\nDo you wish to install the package? (if \"No\", program execution stops if \"No\")")  
    if(ans == 2) stop("package will not be installed")
    install.packages("magrittr")
    rm(ans)
  }
  
  # load packages ----
  if(!any(.packages() %in% "magrittr")) library(magrittr)
  
  # create directory ----
  exdir <- paste0(tempdir(), "/nhis")
  if(!dir.exists(exdir)){
    dir.create(exdir)
  }
  
  # read html ----
  url <- "https://www.cdc.gov/nchs/nhis/"
  query <- paste0("nhis_", year, "_data_release.htm")
  page <- xml2::read_html(paste0(url,query))
  
  # find links with family, person, child, adult ----
  samples <- c("person", "family", "child", "adult")
  patternSamples <- paste0(samples, collapse="|")
  
  links <- page %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    grep(x=., pattern="(?i)\\.zip", value=TRUE) %>% 
    grep(x=., pattern=paste0("(?i)", patternSamples), value=TRUE)
  
  # start downloading files ----
  # samples <- c("familyxxcsv.zip", # name of the zips
  #              "personsxcsv.zip", 
  #              "samchildcsv.zip", 
  #              "samadultcsv.zip")
  # 
  # path <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/"
  # query <- paste0(year, "/", samples)
  # urls <- paste0(path, query)
  # 
  fileNames <- regmatches(x = links, m = regexpr(text = links, pattern = "[[:alpha:]]+\\.zip$"))
  
  # loop over urls for download ----
  for(i in seq_along(links)){
    download.file(links[[i]], destfile = paste0(exdir, "/", fileNames[[i]]), mode = "wb")  
  }
  
  # delete directory and files
  # unlink(exdir, recursive = T)
  
  # unzip files ----
  files <- dir(exdir, full.names = T) # list .zip files
  
  invisible(lapply(files, function(x) unzip(x, exdir = exdir))) # unzip them
}