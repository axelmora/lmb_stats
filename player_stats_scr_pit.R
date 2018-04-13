library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
##functions
#names
name_p <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@name_display_first_last")
  val <- trimws(xml_text(a))
  return(toupper(val))
}
#team
tcp <- function(x){
  #a <- xml_find_all(x, "/boxscore/team/pitching")
  #b <- xml_find_all(x,"/boxscore/team")
  #z <- xml_attr(b,"team_code")
  #ta <- xml_find_all(a[1],"pitcher")
  #th <- xml_find_all(a[2],"pitcher")
  #xml_set_attr(ta,"name",z[1])
  #xml_set_attr(th,"name",z[2])
  #t <- xml_find_all(x,"/boxscore/team/batting/pitcher/@name")
  #val <- trimws(xml_text(t))
  #return(toupper(val))
  y <- xml_find_all(x, "/boxscore/team/pitching")
  a <- xml_find_all(y[1],"pitcher")
  h <- xml_find_all(y[2],"pitcher")
  ta <- xml_attrs(xml_child(x, 3))[["team_code"]]
  th <- xml_attrs(xml_child(x, 4))[["team_code"]]
  xml_set_attr(a,"name",ta)
  xml_set_attr(h,"name",th)
  t <- xml_find_all(x,"/boxscore/team/pitching/pitcher/@name")
  val <- trimws(xml_text(t))
  return(toupper(val))
}
#OUT
out <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@out")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BB
bbp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@bb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#H
hp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@h")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HR
hrp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@hr")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#R
rp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@r")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#ER
er <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@er")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BK
bk <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@bk")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SO
sop <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@so")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#AO
ao <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@ao")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#GO
go <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@go")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BF
bf <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@bf")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#win
w <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "win")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
#loss
l <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "loss")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
#hold
hd <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "hold")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
#save
sv <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "save")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
##master function
py_st_pt <- function(x){
  
  pitcher <- data.frame(Pitcher_Name = name_p(x),
                     #TEAM = tcp(x),
                     OUT = out(x),
                     H = hp(x),
                     HR = hrp(x),
                     R = rp(x),
                     ER = er(x),
                     SO = sop(x),
                     BB = bbp(x),
                     BK = bk(x),
                     W = w(x),
                     L = l(x),
                     HLD = hd(x),
                     SV = sv(x)
  )
  return(pitcher)
}

##TEAM STATS
tm_st_pt <- function(x){
  
  pitcher <- data.frame(
                        TEAM = tcp(x),
                        OUT = out(x),
                        H = hp(x),
                        HR = hrp(x),
                        R = rp(x),
                        ER = er(x),
                        SO = sop(x),
                        BB = bbp(x),
                        BK = bk(x),
                        W = w(x),
                        L = l(x),
                        HLD = hd(x),
                        SV = sv(x)
  )
  return(pitcher)
}