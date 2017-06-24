library(shinydashboard)
library(RCurl)
library(XML)
library(xml2)
library(chron)
library(DT)



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
                  dateInput("date", "Date", value = "2017-05-31"),
                  actionButton("do","Get XMLs"),
                  actionButton("games","Games")
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
  
  teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","vra","yuc","slt","tij")
  
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
      
      for(i in 1:p$n){
        if(input[[paste0('chek',i)]] == TRUE){
          a = 2
        }else{
          a = 1
        }
      } 
      
      res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",
                      substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2017_",substring(input$date,6,7),"_",
                        substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
                          input[[paste0('home', i)]],"aaa_",a,"/rawboxscore.xml")))
        eje <- list()
        for(i in 1:p$n){
          eje[[i]] <- paste(read_xml(as.character(res[i])),input[[paste0('obs', i)]],input[[paste0('chek', i)]])
        }
        
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
    #GET DATE
    date <- function(x){
      a <- xml_find_all(x, "/boxscore/@game_id")
      val <- trimws(xml_text(a))
      val <- substring(val,1,10)
      val <- gsub("/","-",val)
      return(val)
    }
    #GET AWAY TEAM
    awayteam <- function(x){
      a <- xml_find_all(x, "/boxscore/team/@team_code")
      val <- trimws(xml_text(a[1]))
      return(toupper(val))
    }
    #GET HOME TEAM
    hometeam <- function(x){
      a <- xml_find_all(x, "/boxscore/team/@team_code")
      val <- trimws(xml_text(a[2]))
      return(toupper(val))
    }
    #GET ATTENDANCE
    attendance <- function(x){
      a <- xml_find_all(x, "/boxscore/@attendance")
      val <- trimws(xml_text(a))
      val <- as.integer(gsub(",","", val))
      return(val)
    }
    #GET TIME
    time <- function(x){
      a <- xml_find_all(x, "/boxscore/@elapsed_time")
      val <- trimws(xml_text(a))
      val <- substring(val,1,4)
      val <- gsub(" ","",paste(val,":00"))
      val <- 60 * 24 * as.numeric(times(val))
      return(val)
    }
    #GET RUNS SCORED BY HOME TEAM
    home_runs <- function(x){
      a <- xml_find_all(x, "/boxscore/linescore/@home_team_runs")
      val <- trimws(xml_text(a))
      val <- as.integer(val)
      return(val)
    }
    #GET RUNS SCORED BY AWAY TEAM
    away_runs <- function(x){
      a <- xml_find_all(x, "/boxscore/linescore/@away_team_runs")
      val <- trimws(xml_text(a))
      val <- as.integer(val)
      return(val)
    }
    #GET HITS BY HOME TEAM
    home_hits <- function(x){
      a <- xml_find_all(x, "/boxscore/linescore/@home_team_hits")
      val <- trimws(xml_text(a))
      val <- as.integer(val)
      return(val)
    }
    #GET HITS BY AWAY TEAM
    away_hits <- function(x){
      a <- xml_find_all(x, "/boxscore/linescore/@away_team_hits")
      val <- trimws(xml_text(a))
      val <- as.integer(val)
      return(val)
    }
    #ALL TOGHETER IN A DATAFRAME, OBS IS A CHARACTER MANUAL INPUT
    boxscore <- function(x,obs){
      date <- date(x)
      at <- awayteam(x)
      ht <- hometeam(x)
      att <- attendance(x)
      tim <- time(x)
      ar <- away_runs(x)
      ah <- away_hits(x)
      hr <- home_runs(x)
      hh <- home_hits(x)
      
      if(ht == "AGU" || ht == "DUR" || ht == "SLT" || ht == "VAQ" || ht == "MXO" || ht == "TIJ" || ht == "MVA" || ht == "MTY"){
        zone <- "NTE"
      }else if(ht == "CAM" || ht == "LEO" || ht == "OAX" || ht == "PUE" || ht == "QUI" || ht == "TAB" || ht == "VRA" || ht == "YUC"){
        zone <- "SUR"
      }
      game <- data.frame(DATE = date,VISITA=at,LOCAL=ht,ZONA=zone,CV = ar,CL = hr,HV = ah,HL=hh,ATT=att,TIME=tim,OBS=obs)
      return(game)
    }
    
    res <- lapply(1:p$n, function(i) gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_",
                                                       substring(input$date,6,7),"/day_",substring(input$date,9,10),"/gid_2017_",substring(input$date,6,7),"_",
                                                       substring(input$date,9,10),"_",input[[paste0('away', i)]],"aaa_",
                                                       input[[paste0('home', i)]],"aaa_1/rawboxscore.xml")))
    
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Adding to LMB2017', value = 0, {
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
    
    game <- list()
    for(i in 1:p$n){
      game[[i]] <- boxscore(read_xml(as.character(res[i])),input[[paste0('obs', i)]])
      print(game[[i]])
      LMB2017 <- rbind(LMB2017, game[[i]])
      write.csv(LMB2017, file="LMB2017.csv")
    }
    LMB2017 = read.csv("LMB2017.csv")
    LMB2017 <- LMB2017[2:12]
  })
  
  

}

shinyApp(ui, server)

