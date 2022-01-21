library(shiny)
library(shinythemes)

source("guest.R")

shinyUI(fluidPage(
  navbarPage(
    title = 'Menu',
    id = 'navbar',
    windowTitle = 'CSGO statistics and prediction',
    collapsible = TRUE,
    theme = shinytheme('cosmo'),
    
    #home tabPanel
    guestHomeTab,
    #match demo tabPanel
    guestMatchTab,
    #rating demo tabPanel
    guestRatingTab,
    #registration tabPanel
    guestRegistrationTab
  ),
  
  tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Merriweather:ital@1&display=swap');
      body {
        font-family: 'Merriweather', sans-serif;
      }"))
))
