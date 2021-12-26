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
                             p("This page shows the match history"),
                             p("To see detailed map scores choose a row from the
                               table then click the 'Details' button"),
                             actionButton("userMatchDetails", "Details")),
                           mainPanel(
                             DT::dataTableOutput('userMatchTable'))
                         ))
#team tabPanel
userTeamTab <- tabPanel(title = 'Team rating',
                        sidebarLayout(sidebarPanel(
                          p("This page shows the team ranking created with the use
                            of the 'Elo System' and is updated after every match"),
                          p("Plot tab shows the rating history of the teams"),
                          p("To see rating history for a single team choose a row
                            from the table then click the 'Details' button"),
                          p("After pressing the button, plot tab content is changed
                            to the chosen team rating history"),
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
                                p("This page shows the percentage win chance of two chosen teams"),
                                p("Simply choose two teams and prediction will automatically update"),
                                selectInput("team1Name", "Team 1 name:", team[, 2]),
                                selectInput("team2Name", "Team 2 name:", team[-1, 2])),
                                mainPanel(plotOutput('predictionPlot'),
                                          textOutput("selectedTeams"))
                              ))

#user prediction tabPanel
userPredictionGameTab <- tabPanel(title = 'User prediction',
                              sidebarLayout(sidebarPanel(
                                p("This page allows to play a game where user can
                                  try to predict winner of a future match and earn a ranking point"),
                                p("After choosing match in the table pick the team and click 'Confirm' button"),
                                radioButtons("teamChoice", "Choose a team", choices = c("No match selected")),
                                actionButton("predictionComfirm", "Confirm"),
                                htmlOutput("userPredictionGameConfirm")),
                                mainPanel(DT::dataTableOutput('userPredictionGameTable')))
                              )
#user ranking tabPanel
userGameRankingTab <- tabPanel(title = 'User ranking',
                                  sidebarLayout(sidebarPanel(
                                    p("This page shows ranking of users taking part in the 'User Prediction' game")
                                    ),
                                    mainPanel(DT::dataTableOutput('userGameRankingTable')))
)