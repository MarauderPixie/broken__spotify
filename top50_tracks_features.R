# get unique track IDs
trk_str <- unique(top50_tracks$track_id)

# split string to <100 and make 3 calls, because I'm lazy
t1 <- paste0(trk_str[1:50], collapse = ",")
t2 <- paste0(trk_str[51:100], collapse = ",")
t3 <- paste0(trk_str[101:length(trk_str)], collapse = ",")

f1 <- GET("https://api.spotify.com/v1/audio-features", config(token = tokn_me),
          query = list(ids = t1)) %>%
  content() %>%
  .$audio_features

f2 <- GET("https://api.spotify.com/v1/audio-features", config(token = tokn_me),
          query = list(ids = t2)) %>%
  content() %>%
  .$audio_features

f3 <- GET("https://api.spotify.com/v1/audio-features", config(token = tokn_me),
          query = list(ids = t3)) %>%
  content() %>%
  .$audio_features

feats <- c(f1, f2, f3)

# build dataframe
audio_features <- map_df(seq_along(feats), function(x){
  data.frame(
    track_id     = feats[[x]]$id,
    BPM          = feats[[x]]$tempo,
    Danceability = feats[[x]]$danceability,
    Energy       = feats[[x]]$energy,
    Key          = feats[[x]]$key,
    Loudness     = feats[[x]]$loudness,
    Mode         = feats[[x]]$mode,
    Speechiness  = feats[[x]]$speechiness,
    Acousticness = feats[[x]]$acousticness,
    Instrumentalness = feats[[x]]$instrumentalness,
    Liveness     = feats[[x]]$liveness,
    Valence      = feats[[x]]$valence
  )
})

# combine with top 50 tracks df
top50_tracks_complete <- inner_join(top50_tracks, audio_features, by = "track_id")

# save to file
saveRDS(top50_tracks_complete, "./data/top50_tracks.rds")
rm(trk_str, t1, t2, t3, f1, f2, f3, feats, audio_features, top50_tracks_complete)
