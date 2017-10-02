# hauptsache tokn_me aus OAuth.R ist vorhanden
short_l <- GET("https://api.spotify.com/v1/me/top/artists", config(token = tokn_me),
               # "medium_term" = last 6 months; "short_term" ~= 4 weeks, "long_term" == "several years"
               query = list(limit = 30, time_range = "short_term")) %>% content %>% .$items

medium_l <- GET("https://api.spotify.com/v1/me/top/artists", config(token = tokn_me),
                # "medium_term" = last 6 months; "short_term" ~= 4 weeks, "long_term" == "several years"
                query = list(limit = 30, time_range = "medium_term")) %>% content %>% .$items

long_l <- GET("https://api.spotify.com/v1/me/top/artists", config(token = tokn_me),
              # "medium_term" = last 6 months; "short_term" ~= 4 weeks, "long_term" == "several years"
              query = list(limit = 30, time_range = "long_term")) %>% content %>% .$items

# make data usable again
# build function
build_artists <- function(data){
  map_df(seq_along(data), function(x){
    data.frame(
      Position = x,
      Artist   = data[[x]]$name,
      Popularity = data[[x]]$popularity,
      Followers  = data[[x]]$followers$total,
      Genres   = ifelse(length(data[[x]]$genres) == 0, NA,
                        paste(as.character(data[[x]]$genres), collapse = ", ")),
      id       = data[[x]]$id,
      # die images[[4]] ist das kleinste bild; ersetzen mit 1 für das größte (ca. 1000x800px)
      artist_image = ifelse(length(data[[x]]$images) == 0, NA,
                            data[[x]]$images[[3]]$url)
    )
  })
}

# build dataframes
short  <- build_artists(short_l) %>% mutate(time_range = rep("short", length(Artist)))
medium <- build_artists(medium_l) %>% mutate(time_range = rep("medium", length(Artist)))
long   <- build_artists(long_l) %>% mutate(time_range = rep("long", length(Artist)))
rm(short_l, medium_l, long_l)


# combine dataframes, save to file
top30_artists <- rbind(short, medium, long)
saveRDS(top30_artists, "./data/top30_artists.rds")
rm(short, medium, long)
