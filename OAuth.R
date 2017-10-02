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


tokn <- spotifyOAuth("", client_id, client_secret)

tokn_me <- spotifyOAuth("", client_id, client_secret)
