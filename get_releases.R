get_releases <- function(artist.uri, entries = 20, releases = "albums") {

  # first call to get album uris
  rawleases <- map_df(seq_len(length(artist.uri)), function(x) {
    raw <- GET(paste0("https://api.spotify.com/v1/artists/",
                      artist.uri[x], "/albums"),
               # number of releases (entries) and releas type
               # query = list(limit = entries, album_type = releases)) %>%
               query = list(limit = entries)) %>%
      content(as = "text") %>%
      as.tbl_json() %>%
      enter_object("items") %>%
      gather_array() %>%
      spread_all() %>%
      as.data.frame(stringsAsFactors = F)

    return(raw)
  })

  # reduce to aasked for releas.type (if given)
  if (releases == "albums") {
    rawleases <- filter(rawleases, album_type == "album")
  } else {
    if (releases == "singles") {
      rawleases <- filter(rawleases, album_type == "single")
    } else {
      if (releases == "compilations") {
        rawleases <- filter(rawleases, album_type == "compilation")
      }
    }
  }

  # filter duplicates and prepare uris
  uris <- rawleases %>%
    filter(!duplicated(tolower(name))) %>%
    group_by(name) %>%
    summarise(album.id.string = paste0(unlist(id), collapse = ",")) %>%
    .$album.id.string

  # second call to get actual album data
  releases <- suppressWarnings(
    map_df(seq_len(length(uris)), function(x) {
      df <- GET("https://api.spotify.com/v1/albums/",
                query = list(ids = uris[x])) %>%
        content(as = "text") %>%
        as.tbl_json() %>%
        enter_object("albums") %>%
        gather_array() %>%
        spread_values(
          album_type = jstring("album_type"),
          label      = jstring("label"),
          title      = jstring("name"),
          pop        = jnumber("popularity"),
          release    = jstring("release_date"),
          album_id   = jstring("id")
        ) %>%
        enter_object("artists") %>%
        gather_array() %>%
        spread_values(Artist    = jstring("name"),
                      artist_id = jstring("id")) %>%
        as.data.frame(stringsAsFactors = F)

      return(df)
    })
  )

  # create nice and tidy output
  releases <- releases %>%
    transmute(
      Release_Type = album_type,
      Artist       = Artist,
      Album        = title,
      Year         = ifelse(nchar(release) == 4,
                            year(as.Date(release, "%Y")),
                            year(as.Date(release, "%Y-%m-%d"))),
      Label        = label,
      Popularity   = pop,
      artist_id    = artist_id,
      album_id     = album_id
    ) %>%
    arrange(Artist, Year)

  return(releases)
}
