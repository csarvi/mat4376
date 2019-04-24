consolidateData <- function(spotify_client_id=NULL,
                            spotify_client_secret=NULL,
                            spotify_id=NULL,
                            user_name=NULL,
                            playlistName=NULL){
  # package installation check ----
  source("helpers/checkPkg.R", local = T)
  pkgs <- c("magrittr", "data.table", "crayon", "devtools")
  for(i in seq_along(pkgs)){
    checkPkg(pkg=pkgs[[i]])
  }
  rm(i, pkgs)
  
  if(!nzchar(system.file(package = "spotifyr"))){
    ans <- menu(choices = c("Y", "N"),
                title = "Package spotifyr not installed in your system.\n\nDo you wish to install it? (The function will thrown an error if 'N')")
    if(ans == 2L) stop("Execution aborted.")
    devtools::install_github('charlie86/spotifyr')
  }
  
  # check args ----
  # check for args that can't be null first
  if(is.null(user_name) || is.null(playlistName)) stop("You need to supply something to \"user_name\" and/or \"playlistName\".")
  # check for the other args, if null, try to find it in the system
  if(is.null(spotify_client_id) || is.null(spotify_client_secret)){
    if(!nzchar(Sys.getenv("SPOTIFY_CLIENT_ID"))) stop("Supply your spotify client ID to the system: 'Sys.setenv(SPOTIFY_CLIENT_ID='secret_id').")
    if(!nzchar(Sys.getenv("SPOTIFY_CLIENT_SECRET"))) stop("Supply your spotify client secret to the system: 'Sys.setenv(SPOTIFY_CLIENT_SECRET='secret_id').")
  }
  # register id and secret (if id or secret NOT null)
  if(!is.null(spotify_client_id) || !is.null(spotify_client_secret)){
    Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_client_secret)  
  }
  # set id to default id associated with spotify client id and secret
  if(is.null(spotify_id)){
    if(nzchar(Sys.getenv("SPOTIFY_CLIENT_ID")) && nzchar(Sys.getenv("SPOTIFY_CLIENT_SECRET"))) spotify_id <- spotifyr::get_my_profile()[, "id"] # access profile
  }
  
  # checking other args----
  if(missing(user_name)) stop("You need to supply a character vector for \"user_name\".\n\nThis argument creates a column in the final data output with the name of the user (e.g. \"Joe\", \"Cesar\".")
  if(missing(playlistName)) stop("You need to supply a string for \"playlistName\".\n\nThis argument searchs the playlist in the user's playlist library.")
  playlistName <- tolower(playlistName)
  
  # load helpers ----  
  source("data/getPlaylistData.R", local=T) # pull all the data
  
  # load libraries ----
  if(any(.packages() %in% "magrittr")) suppressPackageStartupMessages(library(magrittr))
  if(any(.packages() %in% "data.table")) suppressPackageStartupMessages(library(data.table))

  # get consolidated data from spotify
  master <- getPlaylistData(spotify_id = spotify_id, playlistName = playlistName)
  master[, user_name:=user_name]
  
  return(master)
}