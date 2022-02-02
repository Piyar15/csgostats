library(shiny)
library(DBI)
library(RMySQL)
library(DT)
library(EloRating)
library(ggplot2)
library(stringr)

shinyServer(function(input, output, session) {
  
  #inactivity prevention
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

  #guest home
  
  #credentials assigning
  user <- dbGetQuery(db, 'SELECT * FROM `user`')
  credentials <- loginServer(
    id = "loginUI",
    data = user,
    user_col = "login",
    pwd_col = "password",
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  #logout   
  logout_init <-
    logoutServer(id = "logout",
                 active = reactive(credentials()$user_auth))
  #logout button
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(class = "nav navbar-nav navbar-right",
                 tags$li(
                   div(style = "padding: 10px; padding-top: 4px; padding-bottom: 0;",
                       logoutUI("logout"))
                 ))
  )
  
  #login
  
  #authentication and tab selection for user/admin    
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      removeTab(inputId = "navbar", target = "Home")
      removeTab(inputId = "navbar", target = "Match history demo")
      removeTab(inputId = "navbar", target = "Team rating demo")
      removeTab(inputId = "navbar", target = "Registration")
      type <- credentials()$info$type
      
      if(type == "user"){
        source("user.R")
        appendTab("navbar", userMatchTab, select = TRUE)
        appendTab("navbar", userTeamTab)
        appendTab("navbar", userPredictionTab)
        appendTab("navbar", userPredictionGameTab)
        appendTab("navbar", userGameRankingTab)
      }else if(type == "admin"){
        source("admin.R")
        appendTab("navbar", adminMatchTab, select = TRUE)
        appendTab("navbar", adminResultTab)
        appendTab("navbar", adminTeamTab)
        appendTab("navbar", adminMapTab)
        appendTab("navbar", adminEventTab)
        appendTab("navbar", adminUserTab)
      }
    }
  })
  
  #guest match
  
  #match table
  output$guestMatchTable = DT::renderDataTable(datatable(data = guestMatch[,-1], selection = "single"))
  
  guestMatchDetails <<- 0
  
  #map details
  observeEvent(input$guestMatchDetails, {
    selectedRow <- as.numeric(input$guestMatchTable_rows_selected)
    if (guestMatchDetails == 1) {
      output$guestMapDetails <- renderText("")
      updateActionButton(session, "guestMatchDetails", "Details")
      output$guestMatchTable = DT::renderDataTable(datatable(data = guestMatch[,-1], selection = "single"))
      guestMatchDetails <<- 0
    } else{
      if(length(selectedRow)){
        output$guestMapDetails <- renderText(paste(guestMatch[selectedRow,2],"vs",guestMatch[selectedRow,5]))
        updateActionButton(session, "guestMatchDetails", "Back")
        matchId <- guestMatch[selectedRow, 1]
        mapId <- dbGetQuery(db, paste0(
              "SELECT `id_map_result` FROM `result_detail` WHERE `id_result_detail` = (SELECT `id_result_detail` FROM `match_result` WHERE `id_result` = (SELECT `id_result` FROM `match` WHERE `id_match` = '",
              matchId,
              "'))"))
        mapId <- paste(mapId[, 1], collapse = ", ")
        result <- dbGetQuery(db, paste0(
              "SELECT mr.`map_score_1` As `score1`, mr.`map_score_2` AS `score2`, m.`name` AS `map name` FROM `map_result` AS mr
          LEFT JOIN `map` AS m
          ON mr.id_map=m.id_map
          WHERE `id_map_result` IN (",mapId,")"))
        output$guestMatchTable = DT::renderDataTable(datatable(data = result, selection = "none"))
        guestMatchDetails <<- 1
      }}
  })
  
  #guest team
  
  #team table
  output$guestTeamTable = DT::renderDataTable(datatable(data = eloDf[1:10,], selection = "none"))
  
  #team plot
  output$guestTeamPlot <- renderPlot({
    eloplot(eloResult, ids = eloDf[1:10,1])
  })
  
  #guest registration

  #registration data validation and db insert
  observeEvent(input$comfirm, {
    user <- dbGetQuery(db, 'SELECT * FROM `user`')
    loginValidation <- TRUE %in% (input$login == user$login)
    loginLengthValidation <- nchar(input$login) >= 3 && nchar(input$login) <= 15
    passwordLengthValidation <- nchar(input$password) >= 8 && nchar(input$password) <= 23
    emailFormatValidation <- grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      as.character(input$email),
      ignore.case = TRUE)
    emailValidation <- TRUE %in% (input$email == user$email)
    output$invalidData <- renderText({
      if (!loginLengthValidation)
        paste("Login should have between 3 and 15 characters")
      else if(loginValidation)
        paste("This login is already occupied")
      else if (!passwordLengthValidation)
        paste("Password should have between 8 and 23 characters")
      else if (!emailFormatValidation)
        paste("Please input a valid E-mail address")
      else if(emailValidation)
        paste("This E-mail address is already occupied")
      else{
        dbGetQuery(
          db,
          paste0(
            "INSERT INTO `user`(`id_user`, `login`, `password`, `email`, `type`) VALUES (NUll,'",
            input$login,
            "','",
            input$password,
            "','",
            input$email,
            "','user')"
          )
        )
        updateTabsetPanel(session, "navbar", "Home")
        session$reload()
      }
    })
  })
  
  #user match
  
  #match table
  output$userMatchTable = DT::renderDataTable(datatable(data = userMatch[,-1], selection = "single"))
  
  userMatchDetails <<- 0
  
  #map details
  observeEvent(input$userMatchDetails, {
    selectedRow <- as.numeric(input$userMatchTable_rows_selected)
    if (userMatchDetails == 1) {
      output$userMapDetails <- renderText("")
      updateActionButton(session, "userMatchDetails", "Details")
      output$userMatchTable = DT::renderDataTable(datatable(data = userMatch[,-1], selection = "single"))
      userMatchDetails <<- 0
    } else{
      if(length(selectedRow)){
        output$userMapDetails <- renderText(paste(userMatch[selectedRow,2],"vs",userMatch[selectedRow,5]))
        updateActionButton(session, "userMatchDetails", "Back")
        matchId <- userMatch[selectedRow, 1]
        mapId <-
          dbGetQuery(
            db,
            paste0(
              "SELECT `id_map_result` FROM `result_detail` WHERE `id_result_detail` = (SELECT `id_result_detail` FROM `match_result` WHERE `id_result` = (SELECT `id_result` FROM `match` WHERE `id_match` = '",
              matchId,
              "'))"
            )
          )
        mapId <- paste(mapId[, 1], collapse = ", ")
        result <-
          dbGetQuery(
            db,
            paste0(
              "SELECT mr.`map_score_1` As `score1`, mr.`map_score_2` AS `score2`, m.`name` AS `map name` FROM `map_result` AS mr
          LEFT JOIN `map` AS m
          ON mr.id_map=m.id_map
          WHERE `id_map_result` IN (",
              mapId,
              ")"
            )
          )
        output$userMatchTable = DT::renderDataTable(datatable(data = result, selection = "none"))
        userMatchDetails <<- 1
      }}
  })
  
  #user team
  
  #team table
  output$userTeamTable = DT::renderDataTable(datatable(data = eloDf, selection = "single"))
  
  #team plot
  output$userTeamPlot <- renderPlot({eloplot(eloResult)})

  #team text
  output$userRatingPlot <- renderText("Rating history")
  
  teamDetails <<- 0
  
  #single team details and single team plot 
  observeEvent(input$userTeamDetails, {
    selectedRow <- as.numeric(input$userTeamTable_rows_selected)
    if (teamDetails == 1) {
      output$userRatingTable <- renderText("")
      output$userRatingPlot <- renderText("Rating history")
      updateActionButton(session, "userTeamDetails", "Details")
      output$userTeamTable = DT::renderDataTable(datatable(data = eloDf, selection = "single"))
      output$userTeamPlot <- renderPlot({
        eloplot(eloResult)
      })
      teamDetails <<- 0
    } else{
      if(length(selectedRow)){
        output$userRatingTable <- renderText(paste(eloDf[selectedRow,1],"rating history"))
        output$userRatingPlot <- renderText(paste(eloDf[selectedRow,1],"rating history"))
        updateActionButton(session, "userTeamDetails", "Back")
        selectedTeamName <- eloDf[selectedRow,1]
        selectedTeamName <- str_replace_all(selectedTeamName, " ", ".")
        teamRatings <- data.frame(eloResult[["mat"]])
        selectedTeamRating <- teamRatings[[selectedTeamName]]
        selectedTeamRating <- as.list(selectedTeamRating)
        ratingDate <- t(data.frame(eloResult[["truedates"]]))
        ratingDate  <- as.list(ratingDate)
        teamRatingDf <- do.call(rbind, Map(data.frame, date=ratingDate, rating=selectedTeamRating))
        finalteamRatingDf <- teamRatingDf[complete.cases(teamRatingDf),]
        finalteamRatingDf$date <- as.Date(finalteamRatingDf$date)
        
        output$userTeamTable = DT::renderDataTable(datatable(data = finalteamRatingDf, selection = "none"))
        
        output$userTeamPlot <- renderPlot({
          g <- ggplot(finalteamRatingDf, aes(x=date, y=rating)) + geom_line() + theme_classic()
          g+scale_x_date(date_labels = "%Y %b %d")
        })
        teamDetails <<- 1
      }}
  })
  
  #user prediction

  #prediction update
  observeEvent(input$navbar,{
    if(input$navbar == "Prediction"){
      observe({
        team <- dbGetQuery(db, 'SELECT * FROM `team`')
        choices <- team[,2]
        team1 <- input$team1Name
        team2 <- input$team2Name
        updateSelectInput(session, "team1Name", choices = choices[choices != team2], selected = team1)
        updateSelectInput(session, "team2Name", choices = choices[choices != team1], selected = team2)
        probability <- winprob(extract_elo(eloResult, IDs = team1), extract_elo(eloResult, IDs = team2))
        probability <- round(probability * 100)
        output$selectedTeams <- renderText({
          paste("Team ",team1,"have a",probability,"% chance to win against Team ",team2,"according to the 'Elo System'")
        })
        output$predictionPlot <- renderPlot({
          pie(c(probability, 100 - probability), labels = c(paste(team1," ",probability,"%"), paste(team2," ",100 - probability,"%")), col = rainbow(2), main="Probability chart")
        })
      })
  }})
  
  
  
  #user prediction game
  
  #prediction game table
  output$userPredictionGameTable = DT::renderDataTable(datatable(data = userPredictionGame[,-1], selection = "single"))

  #update radioButton choices / clear text
  observe({
    source("user.R")
    selectedRow <- as.numeric(input$userPredictionGameTable_rows_selected)
    userName <- credentials()$info[["login"]]
    userId <- dbGetQuery(db, paste0("SELECT id_user FROM `user` WHERE login = '",userName,"'"))
    matchId <- as.numeric(userPredictionGame[selectedRow,1])
    chosenTeam <- dbGetQuery(db, paste0("SELECT user_choice FROM `user_prediction` WHERE id_user = '",userId,"' AND id_match = '",matchId,"'"))
    chosenTeamName <- ifelse(chosenTeam == 0, userPredictionGame$team1[selectedRow], userPredictionGame$team2[selectedRow])
    if(length(selectedRow)){
      output$userPredictionGameConfirm <- renderText({
        HTML(paste0("<br/> <br/> Chosen team: ",chosenTeamName))
      })
    updateRadioButtons(session, "teamChoice", choices = c(userPredictionGame$team1[selectedRow],userPredictionGame$team2[selectedRow]))}
  })
  
  #prediction confirm
  observeEvent(input$predictionComfirm, {
    userName <- credentials()$info[["login"]]
    userId <- dbGetQuery(db, paste0("SELECT id_user FROM `user` WHERE login = '",userName,"'"))
    selectedRow <- as.numeric(input$userPredictionGameTable_rows_selected)
    matchId <- as.numeric(userPredictionGame[selectedRow,1])
    userPrediction <- dbGetQuery(db, paste0("SELECT * FROM `user_prediction` WHERE id_user = '",userId,"' AND id_match = '",matchId,"'"))
    userChoice <- ifelse(input$teamChoice == userPredictionGame$team1[selectedRow],0,1)
    if(length(selectedRow)){
      if(dim(userPrediction)[1] == 0){
    dbGetQuery(db, paste0("INSERT INTO `user_prediction`(`id_user_prediction`,`id_user`,`id_match`,`user_choice`) VALUES (NUll,'",userId,"','",matchId,"','",userChoice,"')"))
      }else{
        dbGetQuery(db, paste0("UPDATE `user_prediction` SET user_choice = '",userChoice,"' WHERE id_user = '",userId,"' AND id_match = '",matchId,"'"))
    }
      output$userPredictionGameConfirm <- renderText({
        HTML(paste("<br/> <br/> Prediction confirmed!"))
      })
      }
      })
  
  #user ranking
  
  userRanking <- dbGetQuery(db, "SELECT login, user_score AS 'score' FROM `user`
                            WHERE user_score IS NOT NULL ORDER BY user_score DESC")

  #ranking table
  output$userGameRankingTable = DT::renderDataTable(datatable(data = userRanking, selection = "single"))
  
  #data refresh
  
  observeEvent(input$navbar,{
    if(input$navbar == "Match"){
      team <<- dbGetQuery(db, 'SELECT * FROM `team`')
      event <<- dbGetQuery(db, 'SELECT * FROM `event`')
      updateSelectInput(session, "team1Name", choices = team[,2])
      updateSelectInput(session, "team2Name", choices = team[,2])
      updateSelectInput(session, "matchEventName", choices = event[,2])
    }
    if(input$navbar == "Result"){
      map <<- dbGetQuery(db, 'SELECT * FROM `map`')
      updateSelectInput(session, "mapScoreName", choices = map[,2])
    }
    if(input$navbar != "User prediction"){
      output$userPredictionGameConfirm <- renderText({
        HTML(paste(""))
      })
    }
  })
  
  #admin match
  
  #match table
  output$matchTable = DT::renderDataTable(datatable(data = adminMatch, selection = "single"))
  
  #match data fill in
  observe({
    adminMatch <<- dbGetQuery(
      db,
      'SELECT m.id_match, t1.name AS `team1`, t2.name AS `team2`, e.name AS `event`, date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      ORDER BY id_match DESC'
    )
    selectedRow <- as.numeric(input$matchTable_rows_selected)
    updateSelectInput(session, "team1Name", selected = adminMatch[selectedRow,2])
    updateSelectInput(session, "team2Name", selected = adminMatch[selectedRow,3])
    updateSelectInput(session, "matchEventName", selected = adminMatch[selectedRow,4])
    updateDateInput(session, "date", value = adminMatch[selectedRow,5])
    updateSelectInput(session, "matchType", selected = adminMatch[selectedRow,6])
  })
  
  #match add
  observeEvent(input$matchAdd, {
    team1Id <- dbGetQuery(db, paste0("SELECT id_team FROM `team` WHERE name = '",input$team1Name,"'"))
    team2Id <- dbGetQuery(db, paste0("SELECT id_team FROM `team` WHERE name = '",input$team2Name,"'"))
    eventId <- dbGetQuery(db, paste0("SELECT id_event FROM `event` WHERE name = '",input$matchEventName,"'"))
    dbGetQuery(db, paste0("INSERT INTO `match`(`id_match`,`id_team_1`,`id_team_2`,`id_result`,`id_event`,`date`,`match_type`) VALUES (NUll,'",team1Id,"','",team2Id,"',Null,'",eventId,"','",input$date,"','",input$matchType,"')"))
    #datatable refresh
    adminMatch <<- dbGetQuery(
      db,
      'SELECT m.id_match, t1.name AS `team1`, t2.name AS `team2`, e.name AS `event`, date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      ORDER BY id_match DESC'
    )
    output$matchTable = DT::renderDataTable(datatable(data = adminMatch, selection = "single"))
  })
  
  #match edit
  observeEvent(input$matchEdit, {
    team1Id <- dbGetQuery(db, paste0("SELECT id_team FROM `team` WHERE name = '",input$team1Name,"'"))
    team2Id <- dbGetQuery(db, paste0("SELECT id_team FROM `team` WHERE name = '",input$team2Name,"'"))
    eventId <- dbGetQuery(db, paste0("SELECT id_event FROM `event` WHERE name = '",input$matchEventName,"'"))
    selectedRow <- as.numeric(input$matchTable_rows_selected)
    dbSendQuery(db,paste0("UPDATE `match` SET `id_team_1`='",team1Id,"',`id_team_2`='",team2Id,"',`id_event`='",eventId,"',`date`='",input$date,"',`match_type`='",input$matchType,"' WHERE `id_match`='",adminMatch[selectedRow,1],"'"))
    #datatable refresh
    adminMatch <<- dbGetQuery(
      db,
      'SELECT m.id_match, t1.name AS `team1`, t2.name AS `team2`, e.name AS `event`, date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      ORDER BY id_match DESC'
    )
    output$matchTable = DT::renderDataTable(datatable(data = adminMatch, selection = "single"))
  })
  
  #match delete
  observeEvent(input$matchDelete, {
    selectedRow <- as.numeric(input$matchTable_rows_selected)
    dbGetQuery(db,paste0("DELETE FROM `match` WHERE `id_match`='",adminMatch[selectedRow,1],"'"))
    #datatable refresh
    adminMatch <<- dbGetQuery(
      db,
      'SELECT m.id_match, t1.name AS `team1`, t2.name AS `team2`, e.name AS `event`, date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      ORDER BY id_match DESC'
    )
    output$matchTable = DT::renderDataTable(datatable(data = adminMatch, selection = "single"))
  })
  
  #match to result assigning
  observeEvent(input$matchDetails, {
    adminMatch <<- dbGetQuery(
      db,
      'SELECT m.id_match, t1.name AS `team1`, t2.name AS `team2`, e.name AS `event`, date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      ORDER BY id_match DESC'
    )
    selectedRow <- as.numeric(input$matchTable_rows_selected)
    matchId <<- adminMatch[selectedRow,1]
    updateTabsetPanel(session, "navbar", "Result")
  })
  
  #admin result
  
  #result table
  output$resultTable = DT::renderDataTable(datatable(data = adminResult, selection = "multiple"))
  
  #result data fill in
  observe({
    adminResult <<- dbGetQuery(db, "SELECT mr.`id_map_result`, mr.`map_score_1` As `score 1`, mr.`map_score_2` AS `score 2`, m.`name` AS `map name` FROM `map_result` AS mr
    LEFT JOIN `map` AS m
    ON mr.id_map=m.id_map
    ORDER BY id_map_result DESC")
    selectedRow <- as.numeric(input$resultTable_rows_selected)
    updateNumericInput(session, "team1Score", value = adminResult[selectedRow,2])
    updateNumericInput(session, "team2Score", value = adminResult[selectedRow,3])
    updateSelectInput(session, "mapScoreName", selected = adminResult[selectedRow,4])
  })
  
  #result add
  observeEvent(input$resultAdd, {
    mapId <- dbGetQuery(db, paste0("SELECT id_map FROM `map` WHERE name = '",input$mapScoreName,"'"))
    dbGetQuery(db, paste0("INSERT INTO `map_result`(`id_map_result`, `map_score_1`, `map_score_2`, `id_map`) VALUES (NUll,'",input$team1Score,"','",input$team2Score,"','",mapId,"')"))
    #datatable refresh
    adminResult <<- dbGetQuery(db, "SELECT mr.`id_map_result`, mr.`map_score_1` As `score 1`, mr.`map_score_2` AS `score 2`, m.`name` AS `map name` FROM `map_result` AS mr
    LEFT JOIN `map` AS m
    ON mr.id_map=m.id_map
    ORDER BY id_map_result DESC")
    output$resultTable = DT::renderDataTable(datatable(data = adminResult, selection = "multiple"))
  })
  
  #result edit
  observeEvent(input$resultEdit, {
    mapId <- dbGetQuery(db, paste0("SELECT id_map FROM `map` WHERE name = '",input$mapScoreName,"'"))
    selectedRow <- as.numeric(input$resultTable_rows_selected)
    dbGetQuery(db,paste0("UPDATE `map_result` SET `map_score_1`='",input$team1Score,"',`map_score_2`='",input$team2Score,"',`id_map`='",mapId,"' WHERE `id_map_result`='",adminResult[selectedRow,1],"'"))
    #datatable refresh
    adminResult <<- dbGetQuery(db, "SELECT mr.`id_map_result`, mr.`map_score_1` As `score 1`, mr.`map_score_2` AS `score 2`, m.`name` AS `map name` FROM `map_result` AS mr
    LEFT JOIN `map` AS m
    ON mr.id_map=m.id_map
    ORDER BY id_map_result DESC ")
    output$resultTable = DT::renderDataTable(datatable(data = adminResult, selection = "multiple"))
  })
  
  #result delete
  observeEvent(input$resultDelete, {
    selectedRow <- as.numeric(input$resultTable_rows_selected)
    dbGetQuery(db,paste0("DELETE FROM `map_result` WHERE `id_map_result`='",adminResult[selectedRow,1],"'"))
    #datatable refresh
    adminResult <<- dbGetQuery(db, "SELECT mr.`id_map_result`, mr.`map_score_1` As `score 1`, mr.`map_score_2` AS `score 2`, m.`name` AS `map name` FROM `map_result` AS mr
    LEFT JOIN `map` AS m
    ON mr.id_map=m.id_map
    ORDER BY id_map_result DESC")
    output$resultTable = DT::renderDataTable(datatable(data = adminResult, selection = "multiple"))
  })
  
  #match to result assigning
  observeEvent(input$resultComfirm, {
    adminResult <<- dbGetQuery(db, "SELECT mr.`id_map_result`, mr.`map_score_1` As `score 1`, mr.`map_score_2` AS `score 2`, m.`name` AS `map name` FROM `map_result` AS mr
    LEFT JOIN `map` AS m
    ON mr.id_map=m.id_map
    ORDER BY id_map_result DESC")
     score1 <- 0
     score2 <- 0
    selectedRows <- as.numeric(input$resultTable_rows_selected)
    resultDetailId <- dbGetQuery(db, 'SELECT MAX(id_result_detail) FROM `result_detail`') + 1
    resultDetailId <- as.numeric(resultDetailId[1,1])
    if(is.na(resultDetailId))resultDetailId <- 1
    for (id in selectedRows) {
      if(adminResult[id,2]>adminResult[id,3]){score1 <- 1 + score1}
      else if(adminResult[id,2]<adminResult[id,3]){score2 <- 1 + score2}
      mapResultId <- as.numeric(adminResult[id,1])
      dbGetQuery(db, paste0("INSERT INTO `result_detail`(`id_result_detail`,`id_map_result`) VALUES ('",resultDetailId,"','",mapResultId,"')"))
    }
    dbGetQuery(db, paste0("INSERT INTO `match_result`(`id_result`,`match_score_1`,`match_score_2`,`id_result_detail`) VALUES (Null,'",score1,"','",score2,"','",resultDetailId,"')"))
    resultId <- dbGetQuery(db,paste0("SELECT `id_result` FROM `match_result` WHERE `id_result_detail`='",resultDetailId,"'"))
    dbGetQuery(db,paste0("UPDATE `match` SET `id_result`='",resultId,"' WHERE `id_match`='",matchId,"'"))
    updateTabsetPanel(session, "navbar", "Match")
  
    #update user score
    predictingUsers <- dbGetQuery(db, paste0("SELECT `id_user` FROM `user_prediction` WHERE id_match = '",matchId,"'"))
    if(dim(predictingUsers)[1] != 0)
    for(id in predictingUsers){
      userPrediction <- dbGetQuery(db, paste0("SELECT user_choice FROM `user_prediction` WHERE id_match = '",matchId,"'"))
      userScore <- dbGetQuery(db, paste0("SELECT user_score FROM `user` WHERE id_user = '",id,"'"))
      if(score1>score2 && userPrediction == 0){
        userNewScore <- ifelse(is.na(userScore),1,userScore+1)
        dbGetQuery(db,paste0("UPDATE `user` SET `user_score`='",userNewScore,"' WHERE `id_user`='",id,"'"))
      }else if(score1<score2 && userPrediction == 1){
          userNewScore <- ifelse(is.na(userScore),1,userScore+1)
          dbGetQuery(db,paste0("UPDATE `user` SET `user_score`='",userNewScore,"' WHERE `id_user`='",id,"'"))
      }
    }
    })
  
  #admin team
  
  #team table
  output$teamTable = DT::renderDataTable(datatable(data = team, selection = "single"))
  
  #team data fill in
  observe({
    team <<- dbGetQuery(db, 'SELECT * FROM `team`')
    selectedRow <- as.numeric(input$teamTable_rows_selected)
    updateTextInput(session, "teamName", value = team[selectedRow,2])
  })
  
  #team add
  observeEvent(input$teamAdd, {
    teamNameValidation <- TRUE %in% (input$teamName == team$name)
    if(!teamNameValidation){
      output$occupiedTeamName <- renderText("")
    dbGetQuery(db, paste0("INSERT INTO `team`(`id_team`, `name`) VALUES (NUll,'",input$teamName,"')"))}
    else {output$occupiedTeamName <- renderText({paste("This name is already occupied")})}
    #datatable refresh
    team <<- dbGetQuery(db, 'SELECT * FROM `team`')
    output$teamTable = DT::renderDataTable(datatable(data = team, selection = "single"))
  })
  
  #team edit
  observeEvent(input$teamEdit, {
    selectedRow <- as.numeric(input$teamTable_rows_selected)
    teamNameValidation <- TRUE %in% (input$teamName == team$name)
    if(!teamNameValidation){
    output$occupiedTeamName <- renderText("")
    dbGetQuery(db,paste0("UPDATE `team` SET `name`='",input$teamName, "' WHERE `id_team`='",team[selectedRow,1],"'"))}
    else {output$occupiedTeamName <- renderText({paste("This name is already occupied")})}
    #datatable refresh
    team <<- dbGetQuery(db, 'SELECT * FROM `team`')
    output$teamTable = DT::renderDataTable(datatable(data = team, selection = "single"))
  })
  
  #team delete
  observeEvent(input$teamDelete, {
    selectedRow <- as.numeric(input$teamTable_rows_selected)
    dbGetQuery(db,paste0("DELETE FROM `team` WHERE `id_team`='",team[selectedRow,1],"'"))
    #datatable refresh
    team <<- dbGetQuery(db, 'SELECT * FROM `team`')
    output$teamTable = DT::renderDataTable(datatable(data = team, selection = "single"))
  })
  
  #admin map
  
  #map table
  output$mapTable = DT::renderDataTable(datatable(data = map, selection = "single"))
  
  #map data fill in
  observe({
    map <<- dbGetQuery(db, 'SELECT * FROM `map`')
    selectedRow <- as.numeric(input$mapTable_rows_selected)
    updateTextInput(session, "mapName", value = map[selectedRow,2])
    updateSelectInput(session, "activeDuty", selected = map[selectedRow,3])
  })
  
  #map add
  observeEvent(input$mapAdd, {
    mapNameValidation <- TRUE %in% (input$mapName == map$name)
    if(!mapNameValidation){
    output$occupiedMapName <- renderText("")
    dbGetQuery(db, paste0("INSERT INTO `map`(`id_map`, `name`, `active_duty`) VALUES (NUll,'",input$mapName,"','",input$activeDuty,"')"))}
    else {output$occupiedMapName <- renderText({paste("This name is already occupied")})}
    #datatable refresh
    map <<- dbGetQuery(db, 'SELECT * FROM `map`')
    output$mapTable = DT::renderDataTable(datatable(data = map, selection = "single"))
  })
  
  #map edit
  observeEvent(input$mapEdit, {
    selectedRow <- as.numeric(input$mapTable_rows_selected)
    mapNameValidation <- TRUE %in% (input$mapName == map$name)
    if(!mapNameValidation){
    output$occupiedMapName <- renderText("")
    dbGetQuery(db,paste0("UPDATE `map` SET `name`='",input$mapName,"', `active_duty`='",input$activeDuty,"' WHERE `id_map`='",map[selectedRow,1],"'"))}
    else {output$occupiedMapName <- renderText({paste("This name is already occupied")})}
    #datatable refresh
    map <<- dbGetQuery(db, 'SELECT * FROM `map`')
    output$mapTable = DT::renderDataTable(datatable(data = map, selection = "single"))
  })
  
  #map delete
  observeEvent(input$mapDelete, {
    selectedRow <- as.numeric(input$mapTable_rows_selected)
    dbGetQuery(db,paste0("DELETE FROM `map` WHERE `id_map`='",map[selectedRow,1],"'"))
    #datatable refresh
    map <<- dbGetQuery(db, 'SELECT * FROM `map`')
    output$mapTable = DT::renderDataTable(datatable(data = map, selection = "single"))
  })
  
  #admin event
  
  #event table
  output$eventTable = DT::renderDataTable(datatable(data = event, selection = "single"))
  
  #event data fill in
  observe({
    event <<- dbGetQuery(db, 'SELECT * FROM `event`')
    selectedRow <- as.numeric(input$eventTable_rows_selected)
    updateTextInput(session, "eventName", value = event[selectedRow,2])
    updateSelectInput(session, "eventType", selected = event[selectedRow,3])
  })
  
  #event add
  observeEvent(input$eventAdd, {
    eventNameValidation <- TRUE %in% (input$eventName == event$name)
    if(!eventNameValidation){
    output$occupiedEventName <- renderText("")
    dbGetQuery(db, paste0("INSERT INTO `event`(`id_event`, `name`, `event_type`) VALUES (NUll,'",input$eventName,"','",input$eventType,"')"))}
    else {output$occupiedEventName <- renderText({paste("This name is already occupied")})}
    #datatable refresh
    event <<- dbGetQuery(db, 'SELECT * FROM `event`')
    output$eventTable = DT::renderDataTable(datatable(data = event, selection = "single"))
  })
  
  #event edit
  observeEvent(input$eventEdit, {
    selectedRow <- as.numeric(input$eventTable_rows_selected)
    eventNameValidation <- TRUE %in% (input$eventName == event$name)
    if(!eventNameValidation){
      output$occupiedEventName <- renderText("")
    dbGetQuery(db,paste0("UPDATE `event` SET `name`='",input$eventName,"', `event_type`='",input$eventType,"' WHERE `id_event`='",event[selectedRow,1],"'"))}
    else {output$occupiedEventName <- renderText({paste("This name is already occupied")})}
    #datatable refresh
    event <<- dbGetQuery(db, 'SELECT * FROM `event`')
    output$eventTable = DT::renderDataTable(datatable(data = event, selection = "single"))
  })
  
  #event delete
  observeEvent(input$eventDelete, {
    selectedRow <- as.numeric(input$eventTable_rows_selected)
    dbGetQuery(db,paste0("DELETE FROM `event` WHERE `id_event`='",event[selectedRow,1],"'"))
    #datatable refresh
    event <<- dbGetQuery(db, 'SELECT * FROM `event`')
    output$eventTable = DT::renderDataTable(datatable(data = event, selection = "single"))
  })
  
  #admin user
  
  #user table
  output$userTable = DT::renderDataTable(datatable(data = user, selection = "single"))
  
  #user data fill in
  observe({
    user <<- dbGetQuery(db, 'SELECT * FROM `user`')
    selectedRow <- as.numeric(input$userTable_rows_selected)
    updateTextInput(session, "userLogin", value = user[selectedRow,2])
    updateTextInput(session, "userPassword", value = user[selectedRow,3])
    updateTextInput(session, "userEmail", value = user[selectedRow,4])
    updateSelectInput(session, "userType", selected = user[selectedRow,5])
  })
  
  #user add
  observeEvent(input$userAdd, {
    user <<- dbGetQuery(db, 'SELECT * FROM `user`')
    loginValidation <- TRUE %in% (input$userLogin == user$login)
    loginLengthValidation <- nchar(input$userLogin) >= 3 && nchar(input$userLogin) <= 15
    passwordLengthValidation <- nchar(input$userPassword) >= 8 && nchar(input$userPassword) <= 23
    emailFormatValidation <- grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      as.character(input$userEmail),
      ignore.case = TRUE)
    emailValidation <- TRUE %in% (input$userEmail == user$email)
      if (!loginLengthValidation)
        output$invalidUserData <- renderText({paste("Login should have between 3 and 15 characters")})
      else if(loginValidation)
        output$invalidUserData <- renderText({paste("This login is already occupied")})
      else if (!passwordLengthValidation)
        output$invalidUserData <- renderText({paste("Password should have between 8 and 23 characters")})
      else if (!emailFormatValidation)
        output$invalidUserData <- renderText({paste("Please input a valid E-mail address")})
      else if(emailValidation)
        output$invalidUserData <- renderText({paste("This E-mail address is already occupied")})
      else{
        output$invalidUserData <- renderText("")
        dbGetQuery(db, paste0("INSERT INTO `user`(`id_user`, `login`, `password`, `email`, `type`) VALUES (NUll,'",input$userLogin,"','",input$userPassword,"','",input$userEmail,"','",input$userType,"')"))
        #datatable refresh
        user <<- dbGetQuery(db, 'SELECT * FROM `user`')
        output$userTable = DT::renderDataTable(datatable(data = user, selection = "single"))
        }
  })
  
  #user edit
  observeEvent(input$userEdit, {
    user <<- dbGetQuery(db, 'SELECT * FROM `user`')
    selectedRow <- as.numeric(input$userTable_rows_selected)
    loginValidation <- TRUE %in% (input$userLogin == user$login)
    loginLengthValidation <- nchar(input$userLogin) >= 3 && nchar(input$userLogin) <= 15
    passwordLengthValidation <- nchar(input$userPassword) >= 8 && nchar(input$userPassword) <= 23
    emailFormatValidation <- grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      as.character(input$userEmail),
      ignore.case = TRUE)
    emailValidation <- TRUE %in% (input$userEmail == user$email)
    if (!loginLengthValidation)
      output$invalidUserData <- renderText({paste("Login should have between 3 and 15 characters")})
    else if(loginValidation)
      output$invalidUserData <- renderText({paste("This login is already occupied")})
    else if (!passwordLengthValidation)
      output$invalidUserData <- renderText({paste("Password should have between 8 and 23 characters")})
    else if (!emailFormatValidation)
      output$invalidUserData <- renderText({paste("Please input a valid E-mail address")})
    else if(emailValidation)
      output$invalidUserData <- renderText({paste("This E-mail address is already occupied")})
    else{
      output$invalidUserData <- renderText("")
        dbSendQuery(db,paste0("UPDATE `user` SET `login`='",input$userLogin,"', `password`='",input$userPassword,"', `email`='",input$userEmail,"', `type`='",input$userType,"' WHERE `id_user`='",user[selectedRow,1],"'"))
        #datatable refresh
        user <<- dbGetQuery(db, 'SELECT * FROM `user`')
        output$userTable = DT::renderDataTable(datatable(data = user, selection = "single"))
      }
  })
  
  #user delete
  observeEvent(input$userDelete, {
    selectedRow <- as.numeric(input$userTable_rows_selected)
    dbGetQuery(db,paste0("DELETE FROM `user` WHERE `id_user`='",user[selectedRow,1],"'"))
    #datatable refresh
    user <<- dbGetQuery(db, 'SELECT * FROM `user`')
    output$userTable = DT::renderDataTable(datatable(data = user, selection = "single"))
  })
})

#db disconnect
onStop(function(){dbDisconnect(db)})
