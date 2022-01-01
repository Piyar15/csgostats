library(shiny)
library(DBI)
library(RMySQL)

event <- dbGetQuery(db, 'SELECT * FROM `event`')
map <- dbGetQuery(db, 'SELECT * FROM `map`')

adminMatch <- dbGetQuery(
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

adminResult <- dbGetQuery(db, 'SELECT mr.`id_map_result`, mr.`map_score_1` As `score 1`, mr.`map_score_2` AS `score 2`, m.`name` AS `map name` FROM `map_result` AS mr
LEFT JOIN `map` AS m
ON mr.id_map=m.id_map
ORDER BY id_map_result DESC')

    #match tabPanel
    adminMatchTab <- tabPanel(title = 'Match',
             sidebarLayout(sidebarPanel(
                   h2(strong("Match"), class = "text-center"),
                   selectInput("team1Name", "Team 1 name:", team[,2]),
                   selectInput("team2Name", "Team 2 name:", team[,2]),
                   selectInput("matchEventName", "Event name:", event[,2]),
                   dateInput("date", "Date:"),
                   selectInput("matchType", "Match type:", choices = c("BO1","BO2","BO3","BO5")),
                   actionButton("matchAdd", "Add"),
                   actionButton("matchEdit", "Edit"),
                   actionButton("matchDelete", "Delete"),
                   actionButton("matchDetails", "Details")),
               mainPanel(shinyauthr::loginUI(id = "login"), DT::dataTableOutput('matchTable'))
             ))
    #result tabPanel
    adminResultTab <- tabPanel(title = 'Result',
             sidebarLayout(sidebarPanel(
                   h2(strong("Result"), class = "text-center"),
                   numericInput("team1Score", "Team 1 score:", value = 0),
                   numericInput("team2Score", "Team 2 score:", value = 0),
                   selectInput("mapScoreName", "Map name:", map[,2]),
                   actionButton("resultAdd", "Add"),
                   actionButton("resultEdit", "Edit"),
                   actionButton("resultDelete", "Delete"),
                   actionButton("resultComfirm", "Confirm")),
               mainPanel(DT::dataTableOutput('resultTable'))
             ))
    #team tabPanel
    adminTeamTab <- tabPanel(title = 'Team',
             sidebarLayout(sidebarPanel(
               h2(strong("Team"), class = "text-center"),
                   textInput("teamName", "Team name:"),
                   actionButton("teamAdd", "Add"),
                   actionButton("teamEdit", "Edit"),
                   actionButton("teamDelete", "Delete")),
               mainPanel(DT::dataTableOutput('teamTable'))
             ))
    #map tabPanel
    adminMapTab <- tabPanel(title = 'Map',
             sidebarLayout(sidebarPanel(
                   h2(strong("Map"), class = "text-center"),
                   textInput("mapName", "Map name:"),
                   selectInput(
                     inputId = "activeDuty",
                     label = "Active duty status:",
                     choices = c(0, 1)),
                   actionButton("mapAdd", "Add"),
                   actionButton("mapEdit", "Edit"),
                   actionButton("mapDelete", "Delete")),
               mainPanel(DT::dataTableOutput('mapTable'))
             ))
    #event tabPanel
    adminEventTab <- tabPanel(title = 'Event',
              sidebarLayout(sidebarPanel(
              h2(strong("Event"), class = "text-center"),
              textInput("eventName", "Event name:"),
              selectInput(
                inputId = "eventType",
                label = "Event type:",
                choices = c("Online", "LAN")),
              actionButton("eventAdd", "Add"),
              actionButton("eventEdit", "Edit"),
              actionButton("eventDelete", "Delete")),
      mainPanel(DT::dataTableOutput('eventTable'))
    ))
    #user tabPanel
    adminUserTab <- tabPanel(title = 'User',
              sidebarLayout(sidebarPanel(
              h2(strong("User"), class = "text-center"),
              textInput("userLogin", "User login:"),
              textInput("userPassword", "User password:"),
              textInput("userEmail", "User email:"),
              selectInput(
                inputId = "userType",
                label = "User type:",
                choices = c("admin", "user")),
              actionButton("userAdd", "Add"),
              actionButton("userEdit", "Edit"),
              actionButton("userDelete", "Delete")),
      mainPanel(DT::dataTableOutput('userTable'))
    ))