#Shiny App
library(shiny)
library(tidyverse)
library(DT)
setwd("~/Desktop/curr_Chris/BYUWBB/")

roster <- read.csv("roster.csv")
create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                     ))
}
x <- create_btns(1:15)
roster <- roster %>%
  dplyr::bind_cols(tibble("Buttons" = x))

ui <- shinyUI(
  fluidPage(
    
    titlePanel("Track Stats"),
    fluidRow(
      column(6,
             selectInput("home", h3("Home Team"), 
                         choices = unique(roster$Team))),
      column(6,
             selectInput("away", h3("Away Team"), 
                         choices = unique(roster$Team), selected = unique(roster$Team)[2])),
    ),
    fluidRow(
      column(6,
             h3("Home Team"),
             tableOutput('home_table')
      ),
      column(2,
         h3("DREB"),
         fluidRow(
           column(1,
                  actionButton("plus", "+")),
           column(1),
           column(1,
                  actionButton("minus", "-"))
         ),
         br(),
         br()),
      column(2,
             h3("OREB"),
             fluidRow(
               column(1,
                      actionButton("plus", "+")),
               column(1),
               column(1,
                      actionButton("minus", "-"))
             ),
             br(),
             br())
      ),
    fluidRow(
      column(6, 
             h3("Away Team"),
             tableOutput('away_table')
             ),
      column(2,
             h3("DREB"),
             fluidRow(
               column(1,
                      actionButton("plus", "+")),
               column(1),
               column(1,
                      actionButton("minus", "-"))
             ),
             br(),
             br()),
      column(2,
             h3("OREB"),
             fluidRow(
               column(1,
                      actionButton("plus", "+")),
               column(1),
               column(1,
                      actionButton("minus", "-"))
             ),
             br(),
             br())
    ),
  )
)

server <- shinyServer(function(input, output) {
  
  output$home_table <- renderTable({
    roster[which(roster$Team == input$home),]
    })
  
  
  output$away_table <- renderTable({
    roster[which(roster$Team == input$away),]
    })
})

shinyApp(ui = ui, server = server)
