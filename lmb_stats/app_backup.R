library(shinydashboard)
library(RCurl)
library(XML)
library(xml2)
library(chron)
library(DT)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Controls",
                  width = 3,
                  selectInput("ngames", "Number of games", c(1:16)),
                  dateInput("date", "Date", value = "2018-03-22"),
                  actionButton("do","Get XMLs"),
                  actionButton("games","Games"),
                  actionButton("bat","Bat Stats"),
                  actionButton("pit", "Pit Stats")
                ),
                box(
                  title = "Games",
                  width = 9,
                  uiOutput("ui")
                ),
                box(
                  title = "xlms",
                  width = 12,
                  verbatimTextOutput('test')
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              box(
                title = "LMB 2017",
                width = 12,
                DT::dataTableOutput("LMB2017")
              )
      )
    )
  )
)

server <- function(input, output) {
  LMB2017 = read.csv("LMB2017.csv")
  LMB2017 <- LMB2017[2:12]
  
  p <- reactiveValues()
  observe(p$n <- input$ngames)
  
  teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","lar","yuc","slt","tij")
  
  output$ui <-renderUI({
    if (is.null(input$ngames))
      return()
    
    switch(input$ngames,
           "1"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "2"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "3"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "4"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "5"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "6"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "7"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "8"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "9"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "10"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "11"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "12"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "13"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "14"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "15"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           }),
           "16"  = lapply(1:p$n, function(i) {
             column(3,
                    h3(paste0('Game',i)),
                    selectInput(paste0('away', i), paste0('Away', i),
                                choices = teams),
                    selectInput(paste0('home', i), paste0('Home', i),
                                choices = teams),
                    checkboxInput(paste0('chek',i), "chek", value = FALSE),
                    textInput(paste0('obs',i),"Final"),
                    hr()
             )
           })
    )
  })
  
  output$LMB2017 <- DT::renderDataTable({
    DT::datatable(LMB2017)
  })
  ## output$aways <- renderPrint({
  #  
  # dateX <- reactiveValues()
  #  observe(dateX$mm <- substring(input$date,6,7))
  #  observe(dateX$dd <- substring(input$date,9,10))
  
  ##res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",
  ##          dateX$mm,"/day_",dateX$dd,"/gid_2017_",dateX$mm,"_",dateX$dd,"_",
  ##            input[[paste0('away', i)]],"aaa_",input[[paste0('home', i)]],"aaa_1/rawboxscore.xml")))
  
  #res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",
  #                substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2017_",substring(input$date,6,7),"_",
  #                   substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
  #                      input[[paste0('home', i)]],"aaa_1/rawboxscore.xml")))
  
  # str(setNames(res, paste0('game', 1:p$n)))
  #})
  ##
  
  observeEvent(input$do, {
    
    output$test <- renderPrint({
      
      lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                              substring(input$date,6,7),"/day_",substring(input$date,9,10),
                               "/master_scoreboard.xml"))
      print(lx)
      
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
      
      bx <- read_xml(lx)
      
      res <- gameslist(bx)
      n <- length(res)
      
      gamel <- list()
      for(i in 1:n){
        gamel[[i]] <- read_xml(as.character(res[[i]]))
      }
      print(gamel)
      
     # for(i in 1:p$n){
     #    if(input[[paste0('chek',i)]] == TRUE){
     #    a = 2
     #  }else{
    #    a = 1
     #   }
      #} 
      
    #  res <- lapply(1:p$n, function(i) gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
    #                                                     substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2018_",substring(input$date,6,7),"_",
    #                                                     substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
    #                                                     input[[paste0('home', i)]],"aaa_",a,"/rawboxscore.xml")))
      

     # eje <- list()
    #  for(i in 1:p$n){
    #    eje[[i]] <- paste(read_xml(as.character(res[i])),input[[paste0('obs', i)]],input[[paste0('chek', i)]])
    #  }

      # Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      withProgress(message = 'Import XMLS', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      ##g1 <- read_xml(as.character(res[1]))
      ##as.character(res[1])
      "XMLS importados correctamente"
      
    })
  })
  
  observeEvent(input$games, {
    
    res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2018/month_",
                                                       substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2018_",substring(input$date,6,7),"_",
                                                       substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
                                                       input[[paste0('home', i)]],"aaa_1/rawboxscore.xml")))
    
    
    LMB2018 = read.csv("LMB2018.csv")
    LMB2018 <- LMB2018[2:16]

    game <- list()
    for(i in 1:p$n){
      game[[i]] <- bs(read_xml(as.character(res[i])))
      print(game[[i]])
      LMB2018 <- rbind(LMB2018, game[[i]])
      write.csv(LMB2018, file="LMB2018.csv")
    }
    print(LMB2018)
    
    att_time <- LMB2018 %>%
          group_by(DATE) %>%
          summarise(TIME = mean(TIME, na.rm=TRUE),
                    ATT = mean(ATT, na.rm=TRUE))
    write.csv(att_time, file="LMBatt_time.csv")
    print(att_time)
    
    LMB2018$HOME <- tolower(LMB2018$HOME) 
    LMB2018$AWAY <- tolower(LMB2018$AWAY)
    
    teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","lar","yuc","slt","tij")
    
    for(i in teams){
      
      assign(paste0(i,'x'),rbind(assign(paste0(i,'h'),rename(
        select(
          filter(LMB2018, HOME == i),
          DATE, HOME,HW,HL),
        TEAM = HOME,
        W = HW,
        L = HL)
      ),
      assign(paste0(i,'a'),rename(
        select(
          filter(LMB2018, AWAY == i),
          DATE, AWAY,AW,AL),
        TEAM = AWAY,
        W = AW,
        L = AL)
      )
      )
      )
    }
    
    vaqx <- vaqx[order(vaqx$DATE),]
    tijx <- tijx[order(tijx$DATE),]
    larx <- larx[order(larx$DATE),]
    mxox <- mxox[order(mxox$DATE),]
    mtyx <- mtyx[order(mtyx$DATE),]
    mvax <- mvax[order(mvax$DATE),]
    yucx <- yucx[order(yucx$DATE),]
    quix <- quix[order(quix$DATE),]
    agux <- agux[order(agux$DATE),]
    durx <- durx[order(durx$DATE),]
    leox <- leox[order(leox$DATE),]
    oaxx <- oaxx[order(oaxx$DATE),]
    sltx <- sltx[order(sltx$DATE),]
    puex <- puex[order(puex$DATE),]
    tabx <- tabx[order(tabx$DATE),]
    camx <- camx[order(camx$DATE),]
    
    vaqx$vaq <- ((vaqx$W/(vaqx$W+vaqx$L)))
    tijx$tij <- ((tijx$W/(tijx$W+tijx$L)))
    larx$lar <- ((larx$W/(larx$W+larx$L)))
    mxox$mxo <- ((mxox$W/(mxox$W+mxox$L)))
    mtyx$mty <- ((mtyx$W/(mtyx$W+mtyx$L)))
    mvax$mva <- ((mvax$W/(mvax$W+mvax$L)))
    yucx$yuc <- ((yucx$W/(yucx$W+yucx$L)))
    quix$qui <- ((quix$W/(quix$W+quix$L)))
    agux$agu <- ((agux$W/(agux$W+agux$L)))
    durx$dur <- ((durx$W/(durx$W+durx$L)))
    leox$leo <- ((leox$W/(leox$W+leox$L)))
    oaxx$oax <- ((oaxx$W/(oaxx$W+oaxx$L)))
    sltx$slt <- ((sltx$W/(sltx$W+sltx$L)))
    puex$pue <- ((puex$W/(puex$W+puex$L)))
    tabx$tab <- ((tabx$W/(tabx$W+tabx$L)))
    camx$cam <- ((camx$W/(camx$W+camx$L)))
    
    lmbts <- cbind(vaqx[1],vaqx[5],tijx[5],larx[5],mxox[5],mtyx[5],mvax[5],yucx[5],
                   quix[5],agux[5],durx[5],leox[5],oaxx[5],sltx[5],puex[5],tabx[5],camx[5])
    
    write.csv(lmbts, file="LMBts.csv")
    print(lmbts)
    
    for(i in teams){
      
      assign(paste0(i,'x'),rbind(assign(paste0(i,'h'),rename(
        select(
          filter(LMB2018, HOME == i),
          HOME,hR,aR,HW,HL),
        TEAM = HOME,
        W = HW,
        L = HL,
        R = hR,
        RA = aR)
      ),
      assign(paste0(i,'a'),rename(
        select(
          filter(LMB2018, AWAY == i),
          AWAY,aR,hR,AW,AL),
        TEAM = AWAY,
        W = AW,
        L = AL,
        R = aR,
        RA = hR)
      )
      )
      )
    }
    
    vaqx <- vaqx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    tijx <- tijx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    larx <- larx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    mxox <- mxox %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    mtyx <- mtyx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    mvax <- mvax %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    yucx <- yucx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    quix <- quix %>%
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    agux <- agux %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    durx <- durx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    leox <- leox %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    oaxx <- oaxx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    sltx <- sltx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    puex <- puex %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    tabx <- tabx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
    camx <- camx %>% 
      group_by(TEAM) %>% 
      summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n()) 
    
    lmbstan <- rbind(vaqx,tijx,larx,mxox,mtyx,mvax,yucx,quix,agux,durx,leox,oaxx,sltx,puex,tabx,camx) %>%
      mutate(PCT = ((W/(W+L))),
             RperG = R/GP,
             RAperG = RA/GP,
             WinRatio = W/L,
             RunsRatio = R/RA,
             PCTexp = R**2/(R**2+RA**2),
             Wexp = round(57 * PCTexp),
             Lexp = round(57 - Wexp),
             Rdif = R - RA)
    
    lmbstan <- select(lmbstan,TEAM,GP,W:PCT,WinRatio,PCT:Lexp,RperG:RAperG,RunsRatio,Rdif)
    
    lmbstan_S <- lmbstan %>%
      filter(TEAM %in% c("cam","pue","mxo","oax","qui","tab","leo","yuc")) %>%
      arrange(-PCT)
    
    lmbstan_S$TEAM <- recode(lmbstan_S$TEAM,
                             'cam' = 'Piratas de Campeche',
                             'pue' = 'Pericos de Puebla',
                             'mxo' = 'Diablos Rojos del Mexico',
                             'oax' = 'Guerreros de Oaxaca',
                             'qui' = 'Tigres de Quintana Roo',
                             'tab' = 'Olmecas de Tabasco',
                             'leo' = 'Bravos de Leon',
                             'yuc' = 'Leones de Yucatan')
    lmbstan_N <- lmbstan %>%
      filter(TEAM %in% c("mty","mva","vaq","slt","dur","tij","lar","agu")) %>%
      arrange(-PCT)
    
    lmbstan_N$TEAM <- recode(lmbstan_N$TEAM,
                             'mty' = 'Sultanes de Monterrey',
                             'dur' = 'Generales de Durango',
                             'tij' = 'Toros de Tijuana',
                             'lar' = 'Tecolotes de 2 Laredos',
                             'mva' = 'Acereros de Monclova',
                             'slt' = 'Saraperos de Saltillo',
                             'vaq' = 'Algodoneros Union Laguna',
                             'agu' = 'Rieleros de Aguascalientes')
    
    write.csv(lmbstan, file="LMB2018_stan.csv")
    write.csv(lmbstan_S, file="LMB2018_stan_S.csv")
    write.csv(lmbstan_N, file="LMB2018_stan_N.csv")
    
  })
  
  observeEvent(input$bat, {

    res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2018/month_",
                                                       substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2018_",substring(input$date,6,7),"_",
                                                       substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
                                                       input[[paste0('home', i)]],"aaa_1/rawboxscore.xml")))
  
    bats <- list()
    for(i in 1:p$n){
      bats[[i]] <- py_st_bt(read_xml(as.character(res[i])))
      #print(res[i])
      #print(bats[[i]])
      #LMB2018_bat <- rbind(LMB2018_bat,bats[[i]])
      #LMB2018_bat_UP <- rbind(bats[[i]])
      
      #print(LMB2018_bat_UP)
      #write.csv(LMB2018_bat, file="LMB2018_bat.csv")
    }
    #print(bats)
    LMB2018_bat_UP <- do.call(rbind,bats)
    print(LMB2018_bat_UP)
    
    LMB2018_bat = read.csv("LMB2018_bat.csv")
    LMB2018_bat <- LMB2018_bat[2:15]
    print(LMB2018_bat)
    #print(LMB2018_bat)
    #LMB2018_bat_UP = read.csv("LMB2018_bat_UP.csv")
    #LMB2018_bat_UP <- LMB2018_bat_UP[2:15]
    #LMB2018_bat_UP <- LMB2018_bat_UP
    #print(LMB2018_bat_UP)
    
    LMB2018_bat <- LMB2018_bat %>%
      rbind(., LMB2018_bat_UP) %>%
      group_by(Batter_Name) %>%
      summarise(AB = sum(AB), 
                R = sum(R), 
                H = sum(H),
                D = sum(D),
                Tr = sum(Tr),
                HR = sum(HR),
                RBI = sum(RBI),
                BB = sum(BB),
                SO = sum(SO),
                HBP = sum(HBP),
                SB = sum(SB),
                SF = sum(SF),
                SH = sum(SH))
    
    write.csv(LMB2018_bat, file="LMB2018_bat.csv")
    
    bat_bas <- mutate(LMB2018_bat,
                      PA = AB+BB+SF+SH)
    
    bat_bas <- bat_bas %>% 
      filter(PA > 0)
    
    bat_bas <- mutate(bat_bas,
                      AVG = round((H/AB),3),
                      OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)),3),
                      SLG = round((((1*H)+(2*D)+(3*Tr)+(4*HR))/AB),3),
                      OPS = round((OBP+SLG),3))
    
    write.csv(bat_bas, file="LMB2018_bat_bas.csv")
    print("Estadisticas de bateo completo")
    
  })
  
  observeEvent(input$pit, {

    res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2018/month_",
                                                       substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2018_",substring(input$date,6,7),"_",
                                                       substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
                                                       input[[paste0('home', i)]],"aaa_1/rawboxscore.xml")))
    
    pits <- list()
    for(i in 1:p$n){
      pits[[i]] <- py_st_pt(read_xml(as.character(res[i])))
      #print(res[i])
      #print(bats[[i]])
      #LMB2018_bat <- rbind(LMB2018_bat,bats[[i]])
      #LMB2018_bat_UP <- rbind(bats[[i]])
      
      #print(LMB2018_bat_UP)
      #write.csv(LMB2018_bat, file="LMB2018_bat.csv")
    }
    #print(bats)
    LMB2018_pit_UP <- do.call(rbind,pits)
    LMB2018_pit_UP
    #print(LMB2018_pit_UP)
    
    LMB2018_pit = read.csv("LMB2018_pit.csv")
    LMB2018_pit <- select(LMB2018_pit,Pitcher_Name:SV)
    #print(LMB2018_pit)
    #print(LMB2018_bat)
    #LMB2018_bat_UP = read.csv("LMB2018_bat_UP.csv")
    #LMB2018_bat_UP <- LMB2018_bat_UP[2:15]
    #LMB2018_bat_UP <- LMB2018_bat_UP
    #print(LMB2018_bat_UP)
    
    LMB2018_pit <- LMB2018_pit %>%
      rbind(.,LMB2018_pit_UP) %>%
      group_by(Pitcher_Name) %>%
      summarise(OUT = sum(OUT),
                H = sum(H),
                HR = sum(HR),
                R = sum(R),
                ER = sum(ER),
                SO = sum(SO),
                BB = sum(BB),
                BK = sum(BK),
                W = sum(W),
                L = sum(L),
                HLD = sum(HLD),
                SV = sum(SV))
    
    write.csv(LMB2018_pit, file="LMB2018_pit.csv")
    
    pit_bas <- select(LMB2018_pit, Pitcher_Name, H:SV)
    
    pit_bas <- mutate(pit_bas,
                      IP = round(as.numeric(paste0(trunc(LMB2018_pit$OUT/3),".",
                                            LMB2018_pit$OUT%%3)),2),
                      ERA = round(9*(ER/(LMB2018_pit$OUT/3)),2),
                      WHIP = round((BB+H)/(LMB2018_pit$OUT/3),2))
    
    write.csv(pit_bas, file="LMB2018_pit_bas.csv")
    print(pit_bas)
    
    pit_sab <- select(pit_bas, Pitcher_Name, SO:BB,IP:WHIP)
    
    pit_sab <- mutate(pit_sab,
                      'BB/9' = round(9*(BB/LMB2018_pit$OUT/3),2),
                      'K/9' = round(9*(SO/LMB2018_pit$OUT/3),2))
    
    write.csv(pit_sab, file="LMB2018_pit_sab.csv")
    print("Estadisticas de pitcheo completo")
    
  })  
  
}

shinyApp(ui, server)
