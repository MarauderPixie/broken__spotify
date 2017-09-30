get_tracks <- function(albums) {

  ################################################################################
  # shame- and mindlessly copied from RCharlie.com
  client_id     <- "cbeffa315bf740bdac9bab341799d286"
  client_secret <- "fd8debfa6c4d49599b34984f747e2dcd" # except these ofc
  access_token  <- POST("https://accounts.spotify.com/api/token",
                        accept_json(), authenticate(client_id, client_secret),
                        body   = list(grant_type="client_credentials"),
                        encode = "form", httr::config(http_version=2)) %>%
    content %>%
    .$access_token
  ################################################################################

  # create df with album-strings
  albumwise.ids <- albums %>%
    group_by(Artist) %>%
    summarise(album.id.string = paste0(unlist(album_id), collapse = ","))

  # albumwise call for track names and ids
  all_tracks <- suppressWarnings(
    map_df(
      seq_len(length(albumwise.ids$album.id.string)), function(x) {
        album.tracks <- GET("https://api.spotify.com/v1/albums",
                            query = list(ids = albumwise.ids$album.id.string[x])) %>%
          content(as = "text") %>%
          as.tbl_json() %>%
          enter_object("albums") %>%
          gather_array() %>%
          spread_values(
            album_id    = jstring("id"),
            album_title = jstring("name"),
            release     = jstring("release_date")
          ) %>%
          enter_object("tracks") %>%
          enter_object("items") %>%
          gather_array() %>%
          spread_values(
            track_id    = jstring("id"),
            track_title = jstring("name")
            # pop         = jnumber("popularity") # fix this
            # problem: simplified track object in album-call contains no popularity info
          ) %>%
          enter_object("artists") %>%
          gather_array() %>%
          spread_values(
            Artist    = jstring("name"),
            artist_id = jstring("id")
          )

        album.tracks <- album.tracks %>%
          select(album_id, album_title, track_id, release,
                 track_title, pop, Artist, artist_id)

        return(album.tracks)
      })
  )

  # create dataframe with track-strings
  trackwise.ids <- all_tracks %>%
    group_by(album_title) %>%
    summarise(track.id.string = paste0(unlist(track_id), collapse = ","))

  track_features <- map_df(
    seq_len(length(trackwise.ids$track.id.string)), function(x) {
      raw.info <- GET("https://api.spotify.com/v1/audio-features",
                      query = list(access_token = access_token,
                                   ids = trackwise.ids$track.id.string[x])) %>%
        content(as = "text") %>%
        as.tbl_json() %>%
        enter_object("audio_features") %>%
        gather_array() %>%
        spread_all()

      raw.info <- raw.info %>%
        select(-document.id, -array.index, -type, -uri,
               -track_href, -analysis_url, -time_signature) %>%
        mutate(track_id = id)

      return(raw.info)
    })

  all <- inner_join(all_tracks, track_features,
                    by = c("track_id" = "track_id")) %>%
    filter(!duplicated(id)) %>% # just a work around, I guess
    transmute(
      Artist = Artist,
      Album  = album_title,
      Track  = track_title,
      Year   = ifelse(nchar(release) == 4,
                      year(as.Date(release, "%Y")),
                      year(as.Date(release, "%Y-%m-%d"))),
      # Popularity = pop,
      Duration   = duration_ms,
      BPM        = tempo,
      Mode       = recode(mode, `0` = "minor", `1` = "major"),
      Energy     = energy,
      Valence    = valence,
      Danceability = danceability,
      Instrumentalness = instrumentalness,
      Acousticness = acousticness,
      Speechiness  = speechiness,
      Liveness     = liveness,
      Key          = key, # needs recoding; refer to API-Documentation
      Loudness     = loudness,
      artist_id    = artist_id,
      album_id     = album_id,
      track_id     = track_id
    )

  return(all)
}
