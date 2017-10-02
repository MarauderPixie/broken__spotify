get_tracks_new <- function(albums) {

  ################################################################################
  # shame- and mindlessly copied from RCharlie.com
  client_id     <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  client_secret <- "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" # except these ofc
  access_token  <- POST("https://accounts.spotify.com/api/token",
                        accept_json(), authenticate(client_id, client_secret),
                        body   = list(grant_type="client_credentials"),
                        encode = "form", httr::config(http_version=2)) %>%
    content %>%
    .$access_token
  ################################################################################

  # create df with album-strings
  album_ids <- albums %>%
    group_by(Artist) %>%
    summarise(album_id_string = paste0(unlist(album_id), collapse = ","))


}
