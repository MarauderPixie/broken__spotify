### spotify
# die kann ich einfach bei meiner App nachschauen
client_id     <- "xxxxxxxxxxxxxxxxxxxxxxxxx"
client_secret <- "yyyyyyyyyyyyyyyyyyyyyyyyy" # dran denken, neues token zu basteln!


# damit bekomm ich offenbar ein cooles oAuth-Token, das ich ganz viel weiter verwenden kann
# das ganze ist stumpf geklaut von https://github.com/tiagomendesdantas/Rspotify
spotifyOAuth <- function(app_id, client_id, client_secret){
  spotifyR <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access    = "https://accounts.spotify.com/api/token"
    )
  # "R Web Scraper" ist der name meiner App; die app_id also
  myapp <- httr::oauth_app("R Web Scraper", client_id, client_secret)
  return(httr::oauth2.0_token(spotifyR, myapp,scope = "user-top-read"))}

tokn_me <- spotifyOAuth("", client_id, client_secret)


### genius
#############################
## doesn't work at all atm ##
#############################
client_id     <- "xxxxxxxxxxxxxxxxxxxxxxxxx"
client_secret <- "yyyyyyyyyyyyyyyyyyyyyyyyy"

## function
geniusOAuth <- function(client_id, client_secret){
  geniusR <- httr::oauth_endpoint(
    authorize = "https://api.genius.com/oauth/authorize",
    access    = "https://api.genius.com/oauth/token",
    response_type = "token",
    redirect_uri = "https://github.com/MarauderPixie"
  )
  # "R Scraper" ist der name meiner App; die app_id also
  # rausgenommen: , scope = "user-top-read"
  myapp <- httr::oauth_app("R Scraper", client_id, client_secret)
  return(httr::oauth2.0_token(geniusR, myapp))}

tokn_me2 <- geniusOAuth(client_id, client_secret)


###################################
