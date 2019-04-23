consolidateData <- function(spotify_id, playlistName){
  # load packages (if necessary) ----
  if(any(.packages() %in% "magrittr")) library(magrittr)
  if(any(.packages() %in% "data.table")) library(data.table)
  if(any(.packages() %in% "spotifyr")) library(spotifyr)
  
  # helper lookup ----
  # columns to remove...
  colsToRm <- unique(c("playlist_name", "playlist_uri", "playlist_tracks_url", 
                       "playlist_num_tracks", "snapshot_id", "playlist_img", 
                       "album_name", "track_added_at", "track_preview_url", 
                       "track_open_spotify_url", "is_collaboration", 
                       "album_release_date", "album_popularity", "track_number", 
                       "disc_number", "track_popularity", "track_preview_url", "track_open_spotify_url", 
                       "album_uri", "album_img", "album_type", "artist_uri", "album_release_year", "track_uri"))
  
  # access playlist's songs and artists -----
  playlistName <- tolower(playlistName) # playlist name to lower
  
  myPlaylists <- spotifyr::get_user_playlists(spotify_id) # get list of playlists
  myPlaylists[, "playlist_name"] <- tolower(myPlaylists[["playlist_name"]]) # to lower
  
  uri <- myPlaylists[which(myPlaylists$playlist_name == playlistName),][["playlist_uri"]] # select list of interest
  playlistFt <- spotifyr::get_playlist_audio_features(spotify_id, uri) # get table with playlist audio features
  data.table::setDT(playlistFt) # make into DT
  
  # remove columns ----
  cols <- names(playlistFt)[which(names(playlistFt) %in% colsToRm)]
  playlistFt[, (cols) := NULL] # remove cols
  rm(cols)
  
  return(playlistFt)
}