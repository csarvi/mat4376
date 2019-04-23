# load helpers in data manipulation stage ----
source("data/getPlaylistData.R")
source("data/binnarize.R")

# create a list with params for the users ----
params <-  list(list("12186384264", "jose", "power/jangle pop"), 
     list("csarvi", "cesar", "created in r"), 
     list("melizabethp", "maia", "favs extended"))


# get your client ID and secret as per instructions -----
spotify_client_id = "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
spotify_client_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# get raw data ----
master <- lapply(seq_along(params), 
       function(x) getPlaylistData(spotify_client_id = spotify_client_id, 
                                   spotify_client_secret = spotify_client_secret, 
                                   spotify_id = params[[x]][[1]], 
                                   user_name=params[[x]][[2]], 
                                   playlistName = params[[x]][[3]]))

# stack datasets ----
masterStacked <- do.call(rbind, master)

# binnarize data, remove unimportant variables and standardize some vars ----
out <- binnarize(masterStacked, c("key", "mode", "key_mode"))

saveRDS(out, "data/data.RDS")
