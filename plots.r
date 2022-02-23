library(tidyverse)
# Define function to get features for album, instead of playlist.
# Works up to 100 tracks (limitation of get_track_audio_features()).
# get_album_audio_features <- function(album_id) {
#   req = get_album_tracks(album_id)
#   album_info <- get_track_audio_features(req$id)
#   return(merge(req, album_info, by="id"))
# }
# 
# get_base_linkin_park <- function() {
#   hybrid_theory <- get_album_audio_features("2pKw6GERJVAD61449B1EEM")
#   meteora <- get_album_audio_features("4Gfnly5CzMJQqkUFfoHaP3")
#   minutes_to_midnight <- get_album_audio_features("2tlTBLz2w52rpGCLBGyGw6")
#   a_thousand_suns <- get_album_audio_features("113yjuFZEqkkbuLi4sEBxo")
#   living_things <- get_album_audio_features("4XHIjbhjRmqWlosjj5rqSI")
#   the_hunting_party <- get_album_audio_features("3XB2yloP7l00tEUmaODtVi")
#   one_more_light <- get_album_audio_features("5Eevxp2BCbWq25ZdiXRwYd")
#   
#   return(rbind(hybrid_theory, meteora, minutes_to_midnight,
#                        a_thousand_suns,living_things, the_hunting_party, 
#                        one_more_light))
# }
# 
# get_remix_linkin_park <- function(){
#   recharged <- get_album_audio_features("2FUsvD1bw53HGOjAg56vRD")
#   reanimation <- get_album_audio_features("2NbO8RRVTVEUjHaxUVdMDT")
#   return(rbind(recharged, reanimation))
# }

all_lp_tracks <- get_playlist_audio_features("", "1y9QYGfYFzSO7EF53wwLiB")
# Filter out songs that have been duplicated
# (republished under the 20th anniversary album), along with inbetween-tracks
# that are shorter than a minute as these are not songs.
unq_lp_tracks = distinct(all_lp_tracks, track.name, .keep_all = TRUE) %>% 
  filter(track.duration_ms > 60000, track.album.album_type == "album")

unq_lp_tracks$year_album_published = 
  as.Date(format(as.Date(unq_lp_tracks$track.album.release_date, 
                         format="%Y-%m-%d"),"%Y"), "%Y")

ggplot(unq_lp_tracks, aes(x=year_album_published, y=track.popularity)) +
  labs(title="Linkin Park track popularity over time", x="Year", y="Popularity", color = "Album") +
  geom_point(aes(color=track.album.name)) + 
  geom_smooth()
  
main_albums <- c("Hybrid Theory (Bonus Edition)","Meteora (Bonus Edition)", "Minutes to Midnight",  "A Thousand Suns", "LIVING THINGS","The Hunting Party", "One More Light")
main_album_tracks <- unq_lp_tracks %>% filter(track.album.name %in% main_albums)
a <- ggplot(main_album_tracks, aes(x=year_album_published, y=track.popularity)) +
  labs(title="Linkin Park track popularity over time", x="Year", y="Popularity", color = "Album") +
  geom_point(aes(color=track.album.name)) + 
  geom_smooth()
ggplotly(a)