get_artists <- function(artists, exact = T) {

  ################################################################################
  # shame- and mindlessly copied from RCharlie.com
  # also this is necessary for this call now, it seems
  client_id     <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  client_secret <- "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" # except these ofc
  access_token  <- POST("https://accounts.spotify.com/api/token",
                        accept_json(), authenticate(client_id, client_secret),
                        body   = list(grant_type="client_credentials"),
                        encode = "form", httr::config(http_version=2)) %>%
    content %>%
    .$access_token
  ################################################################################

  bob <- map_df(seq_len(length(artists)), function(x) {
    namen <- GET("https://api.spotify.com/v1/search",
                 query = list(access_token = access_token,
                              q = artists[x], type = "artist")) %>%
      content() %>%
      # enter first level of json
      .$artists %>%
      # enter second level
      # get rid of url and other meta data (next, prev, call size...)
      .$items

    # make everything a little easier for creatinf the dataframe
    entries <- seq_len(length(namen))

    result <- data.frame(
      Artist     = map_chr(entries, function(x) {namen[[x]]$name}),
      Genres     = map_chr(entries, function(x) {paste(unlist(namen[[x]]$genres), collapse = ",")}),
      Followers  = map_chr(entries, function(x) {namen[[x]]$followers$total}),
      Popularity = map_dbl(entries, function(x) {namen[[x]]$popularity}),
      artist_id  = map_chr(entries, function(x) {namen[[x]]$id})
    )

    return(result)
  })

  if (exact == FALSE) {
    return(bob)
  } else {
    alice <- filter(bob, Artist %in% artists)
    return(alice)
  }
}
