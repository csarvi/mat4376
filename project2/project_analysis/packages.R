# require packages for Rmarkdown run
if(!nzchar(system.file(package = "scales"))){
  install.packages("scales")
}
if(!nzchar(system.file(package = "htmltools"))){
  install.packages("htmltools")
}
if(!nzchar(system.file(package = "kableExtra"))){
  install.packages("kableExtra")
}
if(!nzchar(system.file(package = "fs"))){
  install.packages("fs")
}
if(!nzchar(system.file(package = "magrittr"))){
  install.packages("magrittr")
}