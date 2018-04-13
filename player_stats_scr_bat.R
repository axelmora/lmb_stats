library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
##functions
#names
names <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@name_display_first_last")
  val <- trimws(xml_text(a))
  return(toupper(val))
}
#team
tc <- function(x){
  #a <- xml_find_all(x, "/boxscore/team/batting")
  #b <- xml_find_all(x,"/boxscore/team")
  #z <- xml_attr(b,"team_code")
  #ta <- xml_find_all(a[1],"batter")
  #th <- xml_find_all(a[2],"batter")
  #xml_set_attr(ta,"name",z[1])
  #xml_set_attr(th,"name",z[2])
  #t <- xml_find_all(x,"/boxscore/team/batting/batter/@name")
  #val <- trimws(xml_text(t))
  #return(toupper(val))
  y <- xml_find_all(x, "/boxscore/team/batting")
  a <- xml_find_all(y[1],"batter")
  h <- xml_find_all(y[2],"batter")
  ta <- xml_attrs(xml_child(x, 3))[["team_code"]]
  th <- xml_attrs(xml_child(x, 4))[["team_code"]]
  xml_set_attr(a,"name",ta)
  xml_set_attr(h,"name",th)
  t <- xml_find_all(x,"/boxscore/team/batting/batter/@name")
  val <- trimws(xml_text(t))
  return(toupper(val))
}
#AB
ab <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@ab")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#R
r <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@r")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#H
h <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@h")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#d
d <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@d")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#T
t <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@t")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HR
hrb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@hr")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#RBI
rbi <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@rbi")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BB
bb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@bb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SO
so <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@so")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HBP
hbp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@hbp")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SB
sb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter")
  b <- xml_attr(a, "sb")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] > 0){
      b[i]
    }
  }
  b <- as.integer(b)
  return(b)
}
#SF
sf <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@sf")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SAC
sac <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@sac")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#TB
tb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@tb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
##master function
py_st_bt <- function(x){

  batter <- data.frame(Batter_Name = names(x),
                     AB = ab(x),
                     #TEAM = tc(x),
                     R = r(x),
                     H = h(x),
                     D = d(x),
                     Tr = t(x),
                     HR = hrb(x),
                     RBI = rbi(x),
                     BB = bb(x),
                     SO = so(x),
                     HBP = hbp(x),
                     SB = sb(x),
                     SF = sf(x),
                     SH = sac(x)
                     #TB = tb(x)
                     )
  return(batter)
}

##TEAM STATS
tm_st_bt <- function(x){
  
  batter <- data.frame(
                       TEAM = tc(x),
                       AB = ab(x),
                       R = r(x),
                       H = h(x),
                       D = d(x),
                       Tr = t(x),
                       HR = hrb(x),
                       RBI = rbi(x),
                       BB = bb(x),
                       SO = so(x),
                       HBP = hbp(x),
                       SB = sb(x),
                       SF = sf(x),
                       SH = sac(x)
  )
  return(batter)
}