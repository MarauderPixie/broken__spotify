get_releases <- function(artist_id, entries = 20, releases = "album") {

  if (entries > 20) {
    stop("API limit of releases to fetch exceeded - set entries to 20 or less")
  }

  # first call to get album ids
  id_list <- map(seq_len(length(artist_id)), function(x) {
    raw <- GET(paste0("https://api.spotify.com/v1/artists/",
                      artist_id[x], "/albums"),
               # number of releases (entries) and releas type
               # query = list(limit = entries, album_type = releases)) %>%
               query = list(access_token = access_token, limit = entries,
                            album_type = releases)) %>%
      content() %>%
      .$items

    entries <- seq_len(length(raw))

    rel_id <- map_chr(entries, function(x) {raw[[x]]$id})

    return(rel_id)
  })

  # problem: converting breaks the actually used code hereafter, but
  # then there's no removal of duplicates
  # convert to single character-string & remove duplicates
  # id_list <- id_list %>% unlist()
  # id_list <- id_list[!duplicated(id_list)]

  # convert to single character string with each entry
  id_string <- map_chr(seq_len(length(id_list)), function(x) {
    stringster <- paste0(unlist(id_list[x]), collapse = ",")

    return(stringster)
  })

  # second call to get actual album data
  releases <- map_df(seq_len(length(id_string)), function(x) {
    df <- GET("https://api.spotify.com/v1/albums/",
              query = list(access_token = access_token,
                           ids = id_string[x])) %>%
      content() %>%
      .$albums

    entries <- seq_len(length(df))

    result <- data.frame(
      Artist  = map_chr(entries, function(x) {df[[x]]$artists[[1]]$name}),
      Album   = map_chr(entries, function(x) {df[[x]]$name}),
      Release = map_chr(entries, function(x) {df[[x]]$release_date}),
      # Tracks  = map_chr(entries, function(x) {df[[x]]$tracks}),
      Popularity = map_dbl(entries, function(x) {df[[x]]$popularity}),
      Type       = map_chr(entries, function(x) {df[[x]]$album_type}),
      album_id   = map_chr(entries, function(x) {df[[x]]$id})
    ) %>%
      mutate(
        Release = ifelse(nchar(Release) == 4,
                         year(as.Date(Release, "%Y")),
                         year(as.Date(Release, "%Y-%m-%d")))
      )

    return(result)
  })

  return(releases)
}
