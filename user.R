library(shiny)
library(DBI)
library(RMySQL)

userMatch <- dbGetQuery(
  db,
  'SELECT m.id_match, t1.name AS `team1`, ms1.match_score_1 AS `score1`, ms2.match_score_2 AS `score2`, t2.name AS `team2`, e.name AS `event`, date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `match_result` AS ms1
      ON m.id_result=ms1.id_result
      LEFT JOIN `match_result` AS ms2
      ON m.id_result=ms2.id_result
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      WHERE m.id_result IS NOT NULL
      ORDER BY date DESC'
)

userPredictionGame <-
  dbGetQuery(
    db,
    'SELECT m.id_match, t1.name AS `team1`, t2.name AS `team2`, e.name AS `event`,
      date, match_type AS `match type`
      FROM `match` AS m
      LEFT JOIN `team` AS t1
      ON m.id_team_1=t1.id_team
      LEFT JOIN `team` AS t2
      ON m.id_team_2=t2.id_team
      LEFT JOIN `event` AS e
      ON m.id_event=e.id_event
      WHERE id_result IS NULL
      ORDER BY date'
  )

team <- dbGetQuery(db, 'SELECT * FROM `team`')

#match tabPanel
userMatchTab <- tabPanel(title = 'Match history',
                         sidebarLayout(
                           sidebarPanel(
                             htmlOutput("userMatchDescription"),
                             actionButton("userMatchDetails", "Details")),
                           mainPanel(
                             DT::dataTableOutput('userMatchTable'))
                         ))
#team tabPanel
userTeamTab <- tabPanel(title = 'Team rating',
                        sidebarLayout(sidebarPanel(
                          htmlOutput("userTeamDescription"),
                          actionButton("userTeamDetails", "Details")
                        ),
                        mainPanel(
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Table", DT::dataTableOutput('userTeamTable')),
                            tabPanel("Plot", plotOutput('userTeamPlot'))
                          )
                        )))
#prediction tabPanel
userPredictionTab <- tabPanel(title = 'Prediction',
                              sidebarLayout(sidebarPanel(
                                htmlOutput("userPredictionDescription"),
                                selectInput("team1Name", "Team 1 name:", team[, 2]),
                                selectInput("team2Name", "Team 2 name:", team[-1, 2])),
                                mainPanel(plotOutput('predictionPlot'),
                                          textOutput("selectedTeams"))
                              ))

#user prediction tabPanel
userPredictionGameTab <- tabPanel(title = 'User prediction',
                              sidebarLayout(sidebarPanel(
                                htmlOutput("userPredictionGameDescription"),
                                radioButtons("teamChoice", "Choose a team", choices = c(userPredictionGame$team1[1],userPredictionGame$team2[1])),
                                actionButton("predictionComfirm", "Confirm"),
                                htmlOutput("userPredictionGameConfirm")),
                                mainPanel(DT::dataTableOutput('userPredictionGameTable')))
                              )
#user ranking tabPanel
userGameRankingTab <- tabPanel(title = 'User ranking',
                                  sidebarLayout(sidebarPanel(
                                    htmlOutput("userGameRankingDescription")),
                                    mainPanel(DT::dataTableOutput('userGameRankingTable')))
)