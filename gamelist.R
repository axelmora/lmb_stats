library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)

gameid <- function(x){
  a <- xml_find_all(x, "/games/game/@game_data_directory")
  val <- trimws(xml_text(a))
  return(val)
}

leagueid <- function(x){
  a <- xml_find_all(x, "/games/game/@home_league_id")
  val <- trimws(xml_text(a))
  return(val)
}

gameslist <- function(x){
  op <- data.frame(link = paste("http://www.milb.com/gdcross",
                                gameid(x),"/rawboxscore.xml"),
                   liga = leagueid(x))
  
  op <- filter(op,liga == 125)
  
  op <- as.list(gsub(" ","",op$link))
  
  return(op)
}