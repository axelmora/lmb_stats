library(shinydashboard)
library(RCurl)
library(XML)
library(xml2)
library(chron)
library(DT)
library(tidyverse)
library(googledrive)

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
                  dateInput("date", "Date", value = "2018-03-22"),
                  actionButton("do","Get XMLs"),
                  actionButton("games","Games"),
                  actionButton("bat","Players Bat Stats"),
                  actionButton("pit", "Players Pit Stats"),
                  actionButton("tb","Team Bat Stats"),
                  actionButton("tp", "Team Pit Stats")
                ),
                box(
                  title = "Log",
                  width = 9,
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

  output$LMB2017 <- DT::renderDataTable({
    DT::datatable(LMB2017)
  })
 
  observeEvent(input$do, {
    
    output$test <- renderPrint({
      
      lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                              substring(input$date,6,7),"/day_",substring(input$date,9,10),
                               "/master_scoreboard.xml"))
      print(lx)
      bx <- read_xml(lx)
      res <- gameslist(bx)
      print(res)
      n <- length(res)
      gamel <- list()
      for(i in 1:n){
        gamel[[i]] <- read_xml(as.character(res[[i]]))
      }
      #print(gamel)
      
      #pio <- drive_download("~/Ejercicios R/shiny/lmb_statsapp/lmb_stats/LMB2018_bat.csv", overwrite = TRUE)
      #puu <- read.csv(pio$name)
      #write.csv(puu, file="puu.csv")
      #drive_upload("puu.csv","puu.csv")
      
    })
  })
  
  observeEvent(input$games, {
    
    lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                            substring(input$date,6,7),"/day_",substring(input$date,9,10),
                            "/master_scoreboard.xml"))
    bx <- read_xml(lx)
    res <- gameslist(bx)
    n <- length(res)
    
    LMB2018 = read.csv("LMB2018.csv")
    LMB2018 <- LMB2018[2:16]

    game <- list()
    for(i in 1:n){
      game[[i]] <- bs(read_xml(as.character(res[[i]])))
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

    lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                            substring(input$date,6,7),"/day_",substring(input$date,9,10),
                            "/master_scoreboard.xml"))
    bx <- read_xml(lx)
    res <- gameslist(bx)
    n <- length(res)
    
    bats <- list()
    for(i in 1:n){
      bats[[i]] <- py_st_bt(read_xml(as.character(res[[i]])))
    }

    LMB2018_bat_UP <- do.call(rbind,bats)
    print(LMB2018_bat_UP)
    LMB2018_bat = read.csv("LMB2018_bat.csv")
    LMB2018_bat <- LMB2018_bat[2:15]
    print(LMB2018_bat)
    
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

    lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                            substring(input$date,6,7),"/day_",substring(input$date,9,10),
                            "/master_scoreboard.xml"))
    bx <- read_xml(lx)
    res <- gameslist(bx)
    n <- length(res)
    
    pits <- list()
    for(i in 1:n){
      pits[[i]] <- py_st_pt(read_xml(as.character(res[[i]])))
    }

    LMB2018_pit_UP <- do.call(rbind,pits)
    LMB2018_pit_UP
    LMB2018_pit = read.csv("LMB2018_pit.csv")
    LMB2018_pit <- select(LMB2018_pit,Pitcher_Name:SV)
    
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
  
  observeEvent(input$tb, {
    
    lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                            substring(input$date,6,7),"/day_",substring(input$date,9,10),
                            "/master_scoreboard.xml"))
    bx <- read_xml(lx)
    res <- gameslist(bx)
    n <- length(res)
    
    bats <- list()
    for(i in 1:n){
      bats[[i]] <- tm_st_bt(read_xml(as.character(res[[i]])))
    }
    
    LMB2018_TM_bat_UP <- do.call(rbind,bats)
    print(LMB2018_TM_bat_UP)
    LMB2018_TM_bat = read.csv("LMB2018_TM_bat.csv")
    LMB2018_TM_bat <- LMB2018_TM_bat[2:15]
    print(LMB2018_TM_bat)
    
    LMB2018_TM_bat <- LMB2018_TM_bat %>%
      rbind(., LMB2018_TM_bat_UP) %>%
      group_by(TEAM) %>%
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
    
    write.csv(LMB2018_TM_bat, file="LMB2018_TM_bat.csv")
    
    TM_bat_bas <- mutate(LMB2018_TM_bat,
                      PA = AB+BB+SF+SH)
    
    TM_bat_bas <- TM_bat_bas %>% 
      filter(PA > 0)
    
    TM_bat_bas <- mutate(TM_bat_bas,
                      AVG = round((H/AB),3),
                      OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)),3),
                      SLG = round((((1*H)+(2*D)+(3*Tr)+(4*HR))/AB),3),
                      OPS = round((OBP+SLG),3))
    
    write.csv(TM_bat_bas, file="LMB2018_TM_bat_bas.csv")
    print("Estadisticas de bateo completo")
    
  })
  
  observeEvent(input$tp, {
    
    lx <- gsub(" ","",paste("https://gd2.mlb.com/components/game/aaa/year_2018/month_",
                            substring(input$date,6,7),"/day_",substring(input$date,9,10),
                            "/master_scoreboard.xml"))
    bx <- read_xml(lx)
    res <- gameslist(bx)
    n <- length(res)
    
    pits <- list()
    for(i in 1:n){
      pits[[i]] <- tm_st_pt(read_xml(as.character(res[[i]])))
    }
    
    LMB2018_TM_pit_UP <- do.call(rbind,pits)
    LMB2018_TM_pit_UP
    LMB2018_TM_pit = read.csv("LMB2018_TM_pit.csv")
    LMB2018_TM_pit <- select(LMB2018_TM_pit,TEAM:SV)
    
    LMB2018_TM_pit <- LMB2018_TM_pit %>%
      rbind(.,LMB2018_TM_pit_UP) %>%
      group_by(TEAM) %>%
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
    
    write.csv(LMB2018_TM_pit, file="LMB2018_TM_pit.csv")
    
    TM_pit_bas <- select(LMB2018_TM_pit, TEAM, H:SV)
    
    TM_pit_bas <- mutate(TM_pit_bas,
                      IP = round(as.numeric(paste0(trunc(LMB2018_TM_pit$OUT/3),".",
                                                   LMB2018_TM_pit$OUT%%3)),2),
                      ERA = round(9*(ER/(LMB2018_TM_pit$OUT/3)),2),
                      WHIP = round((BB+H)/(LMB2018_TM_pit$OUT/3),2))
    
    write.csv(TM_pit_bas, file="LMB2018_TM_pit_bas.csv")
    print(TM_pit_bas)
    
    TM_pit_sab <- select(TM_pit_bas, TEAM, SO:BB,IP:WHIP)
    
    TM_pit_sab <- mutate(TM_pit_sab,
                      'BB/9' = round(9*(BB/LMB2018_TM_pit$OUT/3),2),
                      'K/9' = round(9*(SO/LMB2018_TM_pit$OUT/3),2))
    
    write.csv(TM_pit_sab, file="LMB2018_TM_pit_sab.csv")
    print("Estadisticas de pitcheo completo")
    
  })  
}

shinyApp(ui, server)
