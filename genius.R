access_token <- "nooo"

# sample code from RCharlie, sorta
genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q='
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', access_token)

  res <- GET(requestURL) %>% content %>% .$response %>% .$hits

  map_df(seq_along(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}


# own code
get_genius_list <- function(data, n_results = 1) {
  baseURL <- 'https://api.genius.com/search?q='

  requestURLs <- map_chr(seq_along(data$Position), function(x){
    paste0(baseURL, gsub(' ', '%20', data$Track[x]), '%20', gsub(' ', '%20', data$Artist[x]),
           '&per_page=', n_results,
           '&access_token=', access_token)
  })

  res <- map(seq_along(requestURLs), function(x){
    GET(requestURLs[x]) %>% content %>% .$response %>% .$hits
  })

  return(res)
}


listen <- map_df(seq_along(li), function(x){
  if (length(li[[x]]) == 0) {
    data.frame(
      Title  = NA,
      Artist = NA
    )
  } else {
    tmp <- li[[x]][[1]]$result

    data.frame(
      Artist = tmp$primary_artist$name,
      Title  = tmp$title,
      artist_id = tmp$primary_artist$id,
      track_id  = tmp$id
    ) %>%
      filter(str_to_upper(Title) %in% str_to_upper(top50m$Track))
    # idea: 1. drop NAs and 2. return message(), which Song could be fetched
    # 1. is necessary even
    # also: drop all entries that do not match with Track in data (eg top50m)
    # str_to_upper(listen$Title) %in% str_to_upper(top50m$Track)
  }
}) %>% filter(!is.na(Title))

listen <- filter(listen, !is.na(track_id))

## test to get a song /o\
song_url <- GET("https://api.genius.com/songs/881774",
                query = list(access_token = access_token)) %>%
  content %>%
  .[[2]] %>%
  .$song %>%
  .$url

## get ALL the songs
song_urls <- map_chr(seq_along(listen$track_id), function(x){
  url <- GET(paste0("https://api.genius.com/songs/", listen$track_id[x]),
                    query = list(access_token = access_token)) %>%
    content %>%
    .[[2]] %>%
    .$song %>%
    .$url

  return(url)
})

# song[[2]]$song$url - enthält die url

lyric_scraper <- function(url) {
  read_html(url) %>%
    html_node("p") %>%
    html_text
}

lyrix <- map_chr(song_urls, lyric_scraper) %>%
  str_replace_all("\\n", " ") %>%
  str_replace_all("\\[[^\\]]*\\]", "") %>%
  str_to_lower() %>%
  str_trim()


# comb the lyrics
# str_replace_all(lyrix[10:13], "\\[Verse [0-9]]", "") %>%
#   str_replace_all("\\[Pre-Chorus]", "") %>%
#   str_replace_all("\\[Chorus x[0-9]]", "") %>%
#   str_replace_all("\\[Chorus]", "") %>%
#   str_replace_all("\\[Outro]", "") %>%
#   str_to_lower()

# should remove all text between square brackets
# str_replace_all(lyrix[1:3], "", "...")
