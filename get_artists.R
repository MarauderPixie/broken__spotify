get_artists <- function(artists, exact = TRUE) {

  bob <- map_df(seq_len(length(artists)), function(x) {
    result <- suppressWarnings(
      GET("https://api.spotify.com/v1/search",
          query = list(access_token = access_token,
                       q = artists[x], type = "artist")) %>%
        content(as = "text") %>%
        as.tbl_json() %>%
        enter_object("artists") %>%
        enter_object("items") %>%
        gather_array() %>%
        spread_all() %>%
        enter_object("genres") %>%
        gather_array() %>%
        append_values_string("Genre") %>%
        mutate(gnrs = paste0(unlist(Genre), collapse = ",")) %>%
        filter(!(duplicated(id))) %>%
        as.data.frame(stringsAsFactors = F)
    )

    return(result)
  })

  bob <- bob %>%
    transmute(
      Artist     = name,
      Popularity = popularity,
      Followers  = followers.total,
      Genres     = gnrs,
      artist_id  = id
    )

  if (exact == FALSE) {
    return(bob)
  } else {
    alice <- filter(bob, Artist %in% artists)
    return(alice)
  }
}
