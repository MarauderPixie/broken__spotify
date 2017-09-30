get_tracks_new <- function(albums) {

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
  album_ids <- albums %>%
    group_by(Artist) %>%
    summarise(album_id_string = paste0(unlist(album_id), collapse = ","))


}
