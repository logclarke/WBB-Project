library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)

##Section 1 ____________________________________________________

#Make sure to write weekly csv and update the cumulative csv in WBB Shiny prep.Rmd

#Read in dataframes that will be used
week_15_game_data <- read.csv("dfweek15.csv")
week_14_game_data <- read.csv("dfweek14.csv")
week_13_game_data <- read.csv("dfweek13.csv")
week_12_game_data <- read.csv("dfweek12.csv")
week_11_game_data <- read.csv("dfweek11.csv")
week_10_game_data <- read.csv("dfweek10.csv")
week_9_game_data <- read.csv("dfweek9.csv")
week_8_game_data <- read.csv("dfweek8.csv")
week_7_game_data <- read.csv("dfweek7.csv")
week_6_game_data <- read.csv("dfweek6.csv")
week_5_game_data <- read.csv("dfweek5.csv")
week_4_game_data <- read.csv("dfweek4.csv")
week_3_game_data <- read.csv("dfweek3.csv")
week_2_game_data <- read.csv("dfweek2.csv")
week_1_game_data <- read.csv("dfweek1.csv")
#this next one needs to be updated every single week
cumulative_game_data <- read.csv("df_cum15.csv")


#We need all this data in a single dataset where we can filter by column name. Here we will add

week_15_game_data <- week_15_game_data %>% 
  mutate(week = "Week 15 Game Data",
         week_num = 15)

week_14_game_data <- week_14_game_data %>% 
  mutate(week = "Week 14 Game Data",
         week_num = 14)

week_13_game_data <- week_13_game_data %>% 
  mutate(week = "Week 13 Game Data",
         week_num = 13)

week_12_game_data <- week_12_game_data %>% 
  mutate(week = "Week 12 Game Data",
         week_num = 12)

week_11_game_data <- week_11_game_data %>% 
  mutate(week = "Week 11 Game Data",
         week_num = 11)

week_10_game_data <- week_10_game_data %>% 
  mutate(week = "Week 10 Game Data",
         week_num = 10)

week_9_game_data <- week_9_game_data %>% 
  mutate(week = "Week 9 Game Data",
         week_num = 9)

week_8_game_data <- week_8_game_data %>% 
  mutate(week = "Week 8 Game Data",
         week_num = 8)

week_7_game_data <- week_7_game_data %>% 
  mutate(week = "Week 7 Game Data",
         week_num = 7)

week_6_game_data <- week_6_game_data %>% 
  mutate(week = "Week 6 Game Data",
         week_num = 6)

week_5_game_data <- week_5_game_data %>% 
  mutate(week = "Week 5 Game Data",
         week_num = 5)

week_4_game_data <- week_4_game_data %>% 
  mutate(week = "Week 4 Game Data",
         week_num = 4)

week_3_game_data <- week_3_game_data %>% 
  mutate(week = "Week 3 Game Data",
         week_num = 3)

week_2_game_data <- week_2_game_data %>% 
  mutate(week = "Week 2 Game Data",
         week_num = 2)

week_1_game_data <- week_1_game_data %>% 
  mutate(week = "Week 1 Game Data",
         week_num = 1)

cumulative_game_data <- cumulative_game_data %>% 
  mutate(week = "Cumulative Game Data",
         week_num = NA)



combined <- rbind(week_1_game_data, week_2_game_data, week_3_game_data, week_4_game_data, week_5_game_data, 
                  week_6_game_data, week_7_game_data, week_8_game_data, week_9_game_data, 
                  week_10_game_data, week_11_game_data, week_12_game_data, week_13_game_data,
                  week_14_game_data, week_15_game_data, cumulative_game_data)

combined$Name[combined$Name == "Paisley Johnson"] <- "Paisley Harding"


##Section 2 ____________________________________________________
#set up the user interface
ui <- navbarPage("Women's Basketball Player Comparison", theme = shinytheme("darkly"),
                 
                # tab with information about the model and the charts
                 tabPanel("About",
                          verbatimTextOutput("About"),
                          fluidPage(
                            tags$b("Application Info", style = "font-size:30px"),
                            br(), #adds an empty line
                            br(),
                            br(),
                            tags$b("Weekly Comparison:", style = "font-size:25px"),
                            br(),
                            br(),
                            tags$ul( #bulleted list
                              tags$li("The Weekly Comparison tab contains a dropdown list of weeks of game 
                                   data (including a cumulative season total), dropdown lists to compare 
                                   two players, and the resulting plots and tables to represent the data. 
                                   This tool will allow for comparison of two players visually and numerically
                                   in the context of the team's performance for a given week.", 
                                      style = "font-size:15px"
                                   )
                            ),
                            br(),
                            tags$b("Season Trends:", style = "font-size:25px"),
                            br(),
                            br(),
                            tags$ul(
                              tags$li("The Season Trends tab dropdown lists to enable the selection and comparison
                                   of two players. The resulting plot shows a line plot showing the season trend of 
                                   each of the selected players along with their season average.", 
                                      style = "font-size:15px"
                                   )
                            ),
                            hr(),
                            tags$b("BPM Interpretation", style = "font-size:25px"),
                            br(),
                            br(),
                            tags$ul(
                              tags$li("The higher (more positive) a player's score is, the better they performed.
                                      While looking at a player's raw BPM is insightful, we find it often an 
                                      even better tool to be able to compare their score to the rest of the team.
                                      This can be a useful tool, especially when comparing players of the same
                                      position to determine playing time and player usage.", 
                                  style = "font-size:15px"
                                  ),
                              tags$li("We should note that in some weeks these player rankings may be slightly
                                    skewed in weeks where both games were blowouts. Our model does not filter out
                                    garbage minutes, so some bench players may have inflated BPM scores in those
                                    weeks. In these cases, looking at the cumulative season data may be useful to 
                                    see overall season trends as opposed to one inflated week",
                                    style = "font-size:15px"
                                    
                            )
                          ),
                          br(),
                          tags$b("Data:", style = "font-size:25px"),
                          br(),
                          br(),
                          tags$ul(
                            tags$li("Game data is collected each week from Synergy Sports.", 
                                   style = "font-size:15px"
                            ),
                            tags$li("Practice data is collected by team managers.",
                                      style = "font-size:15px"
                            )
                          ),
                          br(),
                          br()

                 )
                 ),
                 
                 #tab with the week team charts
                 tabPanel("Weekly Comparison", 
                          
                          fluidPage( #allows layout to fill browser window
                            titlePanel(h1("Player Game Score Comparison", align = "center")),
                            #adds a title to page and browser tab
                            #-use "title = 'tab name'" to name browser tab
                            sidebarPanel( #designates location of following items
                              htmlOutput("data_selector"),#add select input boxes
                              htmlOutput("player_selector1"),  # from objects created in server
                              htmlOutput("player_selector2")# from objects created in server
                            ),
                            
                            mainPanel(
                              plotOutput("plot1"), #put plot item in main area
                              tableOutput("player_table")
                            )
                          ) 
                 ),
                 
                 #tab with the season trend charts
                 tabPanel("Season Trends", 
                          
                          fluidPage( #allows layout to fill browser window
                            titlePanel(h1("Player Season Trendlines", align = "center")),
                            #adds a title to page and browser tab
                            #-use "title = 'tab name'" to name browser tab
                            sidebarPanel( #designates location of following items
                              #  htmlOutput("data_selector"),#add selectinput boxs
                              htmlOutput("player_selector3"),  # from objects created in server
                              htmlOutput("player_selector4") # from objects created in server
                            ),
                            
                            mainPanel(
                              plotOutput("plot2"), #put plot item in main area
                              tableOutput("player_table2")
                            )
                          ) 
                 ),
                
                #tab with model construction info
                tabPanel("Model Construction",
                         fluidPage(
                           tags$b("Model/Game Score Info", style = "font-size:30px"),
                           br(),
                           br(),
                           br(),
                           tags$b("Model Construction:", style = "font-size:25px"),
                           br(),
                           br(),
                           tags$b("To evaluate player performance, we wanted to build a model to predict
                                      the Box Plus/Minus (BPM) as an overall metric of player performance.
                                      Since the data collected in practice contains fewer statistics (especially 
                                      minutes played), we collected data from Synergy Sports and calculated the 
                                      season BPM score for every player on every team in several different conferences. 
                                      We then divided then built a model (a GBM with 3000 trees) to predict the 
                                      BPM score we'd previously calculated using only the stats we'd have from 
                                      practice data.", 
                                     style = "font-size:15px"
                             ),
                           br(),
                           br(),
                           tags$b("Our model tends to weight shooting percentage heavily, especially three-point 
                                      shooting. If a player shoots well from three, their BPM from our model will likely
                                      increase, and vice versa. Each week we total the stats from all games played
                                      during the week, run those player totals through the model to get player
                                      rankings, and update this application with the new dataset.",
                             style = "font-size:15px"
                           ),
                           br(),
                           br(),
                           tags$b("For more information on BPM, visit 
                                     https://www.basketball-reference.com/about/bpm2.html.",
                                  #figure out how to add a link to the shiny app
                                  style = "font-size:15px"
                           )
                         )
                )
                 
)


##Section 3 ____________________________________________________
#server controls what is displayed by the user interface
server = shinyServer(function(input, output) {
  #creates logic behind ui outputs ** pay attention to letter case in names
  
  output$data_selector = renderUI({ #creates data select box object called in ui
    selectInput(inputId = "data_set", #name of input
                label = "Dataset:", #label displayed in ui
                choices = as.character(unique(combined$week)),
                # calls unique values from the State column in the previously created table
                selected = "Cumulative Game Data") #default choice (not required)
  })
  output$player_selector1 = renderUI({#creates player select box object called in ui
    
    data_available = combined[combined$week == input$data_set, c("Name", "BPM_Pred")]
    #creates a reactive list of available players based on the dataset selection made
    
    selectInput(inputId = "player1", #name of input
                label = "Player 1:", #label displayed in ui
                choices = unique(data_available[1]), #calls list of available counties
                selected = "Shaylee Gonzales")
  })
  output$player_selector2 = renderUI({#creates player select box object called in ui
    
    data_available = combined[combined$week == input$data_set, c("Name", "BPM_Pred")]
    #creates a reactive list of available players based on the dataset selection made
    
    selectInput(inputId = "player2", #name of input
                label = "Player 2:", #label displayed in ui
                choices = unique(data_available[1]), #calls list of available counties
                selected = "Paisley Harding")
  })
  
  # added this
  output$player_selector3 = renderUI({#creates player select box object called in ui
    
    selectInput(inputId = "player3", #name of input
                label = "Player 1:", #label displayed in ui
                choices = unique(combined$Name), #calls list of available counties
                selected = "Shaylee Gonzales")
  })
    
    
    #added this too
  output$player_selector4 = renderUI({#creates player select box object called in ui
    
    selectInput(inputId = "player4", #name of input
                label = "Player 2:", #label displayed in ui
                choices = unique(combined$Name), #calls list of available counties
                selected = "Paisley Harding")
  })
  
  output$plot1 = renderPlot({ #creates a the plot to go in the mainPanel
    
    data_available <- combined[combined$week == input$data_set, c("Name", "BPM_Pred")]
    
    data_available %>% 
      ggplot(aes(x = reorder(Name, -BPM_Pred), y = BPM_Pred)) +
      geom_col(fill = ifelse(data_available$Name == input$player1, "royalblue1", 
                             ifelse(data_available$Name == input$player2, "royalblue1", "navy"))) + 
      labs(y = "Box Plus Minus Ranking", x = "Player Name") +
      ggtitle("Player Model Score Comparison") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, face = "bold", size = 10),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.title.x = element_text(face = "bold", size = 14),
            legend.justification = c("right", "top")) +
      geom_hline(yintercept = mean(data_available$BPM_Pred), linetype = 'dotted', color = "red") +
      geom_label(aes(x = nrow(data_available) - 1, y = mean(BPM_Pred), 
                     label = "Team Average", vjust = -0.5), size = 5)
  })
  
  output$player_table <- renderTable({
    
    data_available = combined[combined$week == input$data_set, c("Name", "BPM_Pred")]
    
    bpm1 <- data_available %>% 
      filter(Name == input$player1) %>% 
      select(BPM_Pred)
    
    #isolate just the score. We don't want the column name here
    bpm1 <- bpm1[1,]
    
    bpm2 <- data_available %>% 
      filter(Name == input$player2) %>% 
      select(BPM_Pred)
    
    #isolate just the score. We don't want the column name here
    bpm2 <- bpm2[1,]
    
    team_avg <- mean(data_available$BPM_Pred)
    
    ovr_undr1 <- ifelse(bpm1 >= 0, bpm1- team_avg, - (team_avg - bpm1))
    
    ovr_undr2 <- ifelse(bpm2 >= 0, bpm2- team_avg, - (team_avg - bpm2))
    
    stats <- bind_cols("Name" = c(input$player1, input$player2), 
                       "Box Plus Minus" = c(bpm1, bpm2), 
                       "Difference from Mean" = c(ovr_undr1, ovr_undr2))
    
    colnames(stats) <- c("Name", 
                         "Box Plus Minus", 
                         "Difference Above/Below Team Average")
    
    stats
    
  })
  
  output$plot2 = renderPlot({ #creates a the plot to go in the mainPanel
    
    combined %>% 
      filter(Name == input$player3 | Name == input$player4) %>% 
      ggplot(aes(x = week_num, y = BPM_Pred, col = Name)) +
      geom_line() +
      labs(y = "BPM Score", x = "Week") +
      ggtitle("Player Rankings Through Time") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 15)) +
      #need to update the x each week
      scale_x_continuous(breaks = seq(1, 15, 1)) +
      scale_y_continuous(breaks = seq(round(min(combined$BPM_Pred)), round(max(combined$BPM_Pred)), 2)) +
      scale_color_manual(values=c('navy','royalblue1'))
  }) 
  
  output$player_table2 <- renderTable({

    mean1 <- combined %>% 
      filter(Name == input$player3) %>% 
      summarise(mean = mean(BPM_Pred))
    
    mean1 <- mean1[1,]
    
    mean2 <- combined %>% 
      filter(Name == input$player4) %>% 
      summarise(mean = mean(BPM_Pred))
    
    mean2 <- mean2[1,]
    
    table <- bind_cols("Name" = c(input$player3, input$player4),
                       "Average Weekly Score" = c(mean1, mean2))
    
    table
  })
  
  
  
  
})#close the shinyServer

##Section 4____________________________________________________
shinyApp(ui = ui, server = server) #need this if combining ui and server into one file.

