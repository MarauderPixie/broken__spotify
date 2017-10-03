# merge spotify and genius data
## feels inefficient, maybe think about that later
top50l_filtered <- filter(top50l, str_to_lower(Track) %in% str_to_lower(lyrics$Track))

top50l_filtered$lower_track <- str_to_lower(top50l_filtered$Track)
lyrics$lower_track <- str_to_lower(lyrics$Track)

lyrics_reduced <- lyrics %>% select(lyrics, lower_track)

semi_final <- inner_join(top50l_filtered, lyrics_reduced, by = "lower_track") %>% select(-lower_track)


## on to sentiment analysis!
sad <- sentiments %>%
  filter(lexicon == 'nrc', sentiment == 'sadness') %>%
  select(word) %>%
  mutate(sad = T)

angry <- sentiments %>%
  filter(lexicon == 'nrc', sentiment == 'anger') %>%
  select(word) %>%
  mutate(angry = T)

joy <- sentiments %>%
  filter(lexicon == 'nrc', sentiment == 'joy') %>%
  select(word) %>%
  mutate(joy = T)


# magic
sentimentals <- semi_final %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(sad, by = 'word') %>%
  left_join(angry, by = 'word') %>%
  left_join(joy, by = 'word') %>%
  group_by(Track) %>%
  summarise(
    words = n(),
    Sadness = round(sum(sad, na.rm = T) / n(), 4),
    Joy     = round(sum(joy, na.rm = T) / n(), 4),
    Anger   = round(sum(angry, na.rm = T) / n(), 4)
  ) %>%
  ungroup()

# ok, cool. need to merge now. /sigh
final_l <- inner_join(semi_final, sentimentals, by = "Track") %>%
  mutate(
    Lyrical_density = words / Dur_sec,
    Gloom_Index    = ((1 - Valence) + Sadness * (1 + Lyrical_density)) / 2,
    # these two are not quite right...
    Contend_Index  = ((1 - Danceability) + Joy * (1 + Lyrical_density)) / 2,
    Volatile_Index = ((1 - Energy) + Anger * (1 + Lyrical_density)) / 2
  )

# cleanup
rm(angry, joy, lyrics, lyrics_reduced, sad, semi_final, sentimentals, top50l, top50l_filtered)
