# hauptsache tokn_me aus OAuth.R ist vorhanden
short_l <- GET("https://api.spotify.com/v1/me/top/tracks", config(token = tokn_me),
               # "medium_term" = last 6 months; "short_term" ~= 4 weeks, "long_term" == "several years"
               query = list(limit = 50, time_range = "short_term")) %>% content %>% .$items

medium_l <- GET("https://api.spotify.com/v1/me/top/tracks", config(token = tokn_me),
                # "medium_term" = last 6 months; "short_term" ~= 4 weeks, "long_term" == "several years"
                query = list(limit = 50, time_range = "medium_term")) %>% content %>% .$items

long_l <- GET("https://api.spotify.com/v1/me/top/tracks", config(token = tokn_me),
             # "medium_term" = last 6 months; "short_term" ~= 4 weeks, "long_term" == "several years"
             query = list(limit = 50, time_range = "long_term")) %>% content %>% .$items

# make data usable again
# build function
build_songs <- function(data){
  map_df(seq_along(data), function(x){
    data.frame(
      Position = x,
      Artist   = data[[x]]$artists[[1]]$name,
      Track    = data[[x]]$name,
      Album    = data[[x]]$album$name,
      Dur_sec  = data[[x]]$duration_ms / 1000,
      track_id = data[[x]]$id,
      album_image = ifelse(length(data[[x]]$album$images) == 0, NA,
                           data[[x]]$album$images[[3]]$url)
    )
  })
}

# build dataframes
short  <- build_songs(short_l) %>% mutate(time_range = rep("short", length(Track)))
medium <- build_songs(medium_l) %>% mutate(time_range = rep("medium", length(Track)))
long   <- build_songs(long_l) %>% mutate(time_range = rep("long", length(Track)))
rm(short_l, medium_l, long_l)


# combine dataframes, save to file
top50_tracks <- rbind(short, medium, long)
saveRDS(top50_tracks, "./data/top50_tracks.rds")
rm(short, medium, long)
