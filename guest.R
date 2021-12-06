library(shinyauthr)
library(DBI)
library(RMySQL)
library(EloRating)
library(stringr)

db <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "y0zlx2cmef3wsvlh",
  host = "r6ze0q02l4me77k3.chr7pe7iynqr.eu-west-1.rds.amazonaws.com",
  username = "xjiuy6rmzs2l3vvz",
  password = "x6b1rv2e2u3puqce",
  port = 3306
)

user <- dbGetQuery(db, 'SELECT * FROM `user`')
team <- dbGetQuery(db, 'SELECT * FROM `team`')

guestMatch <- dbGetQuery(
  db,
  'SELECT m.id_match, t1.name AS `team1`, ms1.match_score_1 AS `score1`,
  ms2.match_score_2 AS `score2`, t2.name AS `team2`, e.name AS `event`,
  date, match_type AS `match type`
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
      WHERE DATEDIFF(CURDATE(),date) < 31
      ORDER BY date DESC'
)

guestHomeTab <- tabPanel(title = 'Home',
                         sidebarLayout(sidebarPanel(
                           htmlOutput("guestHomeDescription")),
                           mainPanel(shinyauthr::loginUI(id = "loginUI"))
                         ))

guestMatchTab <- tabPanel(title = 'Match history demo',
                          sidebarLayout(
                            sidebarPanel(
                              htmlOutput("guestMatchDescription"),
                              actionButton("guestMatchDetails", "Details")
                            ),
                            mainPanel(DT::dataTableOutput('guestMatchTable'))
                          ))

guestRatingTab <- tabPanel(title = 'Team rating demo',
                           sidebarLayout(sidebarPanel(htmlOutput(
                             "guestTeamDescription"
                           ),),
                           mainPanel(
                             tabsetPanel(
                               type = "tabs",
                               tabPanel("Table", DT::dataTableOutput('guestTeamTable')),
                               tabPanel("Plot", plotOutput('guestTeamPlot'))
                             )
                           )))

guestRegistrationTab <- tabPanel(title = 'Registration',
                                 sidebarLayout(
                                   sidebarPanel(htmlOutput("guestRegistration"),),
                                   mainPanel(
                                     textInput("login", "Login: (3 to 15 characters)"),
                                     textInput("password", "Password: (8 to 23 characters)"),
                                     textInput("email", "Email:"),
                                     actionButton("comfirm", "Confirm"),
                                     textOutput("invalidData")
                                   )
                                 ))

#Elo Rating
eloData <-dbGetQuery(
  db,
  "SELECT date,
  CASE WHEN ms1.match_score_1 > ms2.match_score_2 THEN t1.name ELSE t2.name
  END AS winner,
  CASE WHEN ms1.match_score_1 > ms2.match_score_2 THEN t2.name ELSE t1.name
  END AS loser
  FROM `match` AS m
  LEFT JOIN `team` AS t1
  ON m.id_team_1=t1.id_team
  LEFT JOIN `match_result` AS ms1
  ON m.id_result=ms1.id_result
  LEFT JOIN `match_result` AS ms2
  ON m.id_result=ms2.id_result
  LEFT JOIN `team` AS t2
  ON m.id_team_2=t2.id_team
  ORDER BY date")

eloResult <- elo.seq(winner = eloData$winner, loser = eloData$loser, Date = eloData$date, k = 32)
eloDf <- extract_elo(eloResult)
eloDf <- t(data.frame(as.list(eloDf)))
eloDf <- data.frame(str_replace_all(rownames(eloDf), "[.]", " "), eloDf)
rownames(eloDf) <- NULL
colnames(eloDf) <- c("team", "rating")