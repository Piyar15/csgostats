library(shiny)
library(shinyauthr)
library(DBI)
library(RMySQL)

db <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "project",
  host = "127.0.0.1",
  username = "root"
)

user <- dbGetQuery(db, 'SELECT `login`,`password`,`type` FROM `user`')



ui <- fluidPage(
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  # setup table output to show user info after login
  tableOutput("user_table")
)


server <- function(input, output) {

  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user,
    user_col = "login",
    pwd_col = "password",
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })
   
}

shinyApp(ui = ui, server = server)