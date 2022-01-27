library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)


#Read in dataframes that will be used
dfweek10_shiny <- read.csv("dfweek10.csv")
dfweek9_shiny <- read.csv("dfweek9.csv")
dfweek8_shiny <- read.csv("dfweek8.csv")
dfweek7_shiny <- read.csv("dfweek7.csv")
dfweek6_shiny <- read.csv("dfweek6.csv")
dfweek5_shiny <- read.csv("dfweek5.csv")
dfweek4_shiny <- read.csv("dfweek4.csv")
dfweek3_shiny <- read.csv("dfweek3.csv")
dfweek2_shiny <- read.csv("dfweek2.csv")
dfweek1_shiny <- read.csv("dfweek1.csv")
#this next one needs to be updated every single week
df_cum_shiny <- read.csv("df_cum.csv")




ui <- fluidPage(
  titlePanel(h1("Box Plus Minus Comparison", align = "center")),
  sidebarPanel(selectInput(
    input = "player1",
    label = "Player 1 Name",
    choices = dfweek10_shiny$Name
  )),
  sidebarPanel(selectInput(
    input = "player2",
    label = "Player 2 Name",
    choices = dfweek10_shiny$Name
  )),
  mainPanel(
    plotOutput(outputId = "player_plot"),
    
    tableOutput(outputId = "player_table"))
)



week_function2 <- function(player1, player2) {
  plot <- ggplot(dfweek10_shiny, aes(x = reorder(Name, -BPM_Pred), y = BPM_Pred)) +
    geom_col(fill = ifelse(dfweek10_shiny$Name == player1, "royalblue1", 
                           ifelse(dfweek10_shiny$Name == player2,"royalblue1","navy"))) + 
    labs(y = "Box Plus Minus Ranking", x = "Player Name") +
    ggtitle("Week 10 Player Rankings") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 14)) +
    geom_hline(yintercept = mean(dfweek10_shiny$BPM_Pred), linetype = 'dotted', color = "red") +
    annotate("text", x = nrow(dfweek10_shiny) - 1, y = mean(dfweek10_shiny$BPM_Pred), 
             label = "Team Average", vjust = -0.5)
  
  bpm1 <- dfweek10_shiny %>% 
    filter(Name == player1) %>% 
    select(BPM_Pred)
  
  #isolate just the score. We don't want the column name here
  bpm1 <- bpm1[1,]
  
  bpm2 <- dfweek10_shiny %>% 
    filter(Name == player2) %>% 
    select(BPM_Pred)
  
  #isolate just the score. We don't want the column name here
  bpm2 <- bpm2[1,]
  
  team_avg <- mean(dfweek10_shiny$BPM_Pred)
  
  ovr_undr1 <- ifelse(bpm1 >= 0, bpm1- team_avg, - (team_avg - bpm1))
  
  ovr_undr2 <- ifelse(bpm2 >= 0, bpm2- team_avg, - (team_avg - bpm2))
  
  stats <- bind_cols("Name" = c(player1, player2), 
                     "Box Plus Minus" = c(bpm1, bpm2), 
                     "Difference from Mean" = c(ovr_undr1, ovr_undr2))
  
  colnames(stats) <- c("Name", 
                       "Box Plus Minus", 
                       "Difference Above/Below Team Average")
  
  out <- list(plot, stats)
  
  return(out)
  
}



server <- function(input, output){
  
  results <- reactive(week_function2(input$player1, input$player2))
  
  output$player_plot <- renderPlot({
    results()[[1]]
  })
  
  output$player_table <- renderTable({
    results()[[2]]
  })
  
  
}




shinyApp(ui, server)






