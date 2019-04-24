# load helpers in data manipulation stage ----
source("data/consolidateData.R")
source("data/binarize.R")

# check pkg requirement ----
if(!nzchar(system.file(package = "spotifyr"))){
  ans <- menu(choices = c("Y", "N"),
              title = "Package spotifyr not installed in your system.\n\nDo you wish to install it? (The function will thrown an error if 'N')")
  if(ans == 2L) stop("Execution aborted.")
  devtools::install_github('charlie86/spotifyr')
}

# create a list with params for the users ----
params <-  list(list("12186384264", "jose", "power/jangle pop"), 
     list("csarvi", "cesar", "created in r"), 
     list("melizabethp", "maia", "favs extended"))


# get your client ID and secret as per instructions -----
spotify_client_id = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
spotify_client_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
# run authorization code
spotifyr::get_spotify_authorization_code()

# get raw data ----
master <- lapply(seq_along(params), 
       function(x) consolidateData(spotify_client_id = spotify_client_id, 
                                   spotify_client_secret = spotify_client_secret, 
                                   spotify_id = params[[x]][[1]], 
                                   user_name=params[[x]][[2]], 
                                   playlistName = params[[x]][[3]]))

# stack datasets ----
masterStacked <- do.call(rbind, master)

#  binarize data, remove unimportant variables and standardize some vars ----
out <- binarize(masterStacked, c("key", "mode", "key_mode"))

saveRDS(out, "data/data.RDS")
