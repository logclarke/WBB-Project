library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)

##Section 1 ____________________________________________________

#Make sure to write weekly csv and update the cumulative csv in WBB Shiny prep.Rmd

#Read in dataframes that will be used
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
cumulative_game_data <- read.csv("df_cum13.csv")


#We need all this data in a single dataset where we can filter by column name. Here we will add

week_13_game_data <- week_13_game_data %>% 
  mutate(week = "Week 13 Game Data")

week_12_game_data <- week_12_game_data %>% 
  mutate(week = "Week 12 Game Data")

week_11_game_data <- week_11_game_data %>% 
  mutate(week = "Week 11 Game Data")

week_10_game_data <- week_10_game_data %>% 
  mutate(week = "Week 10 Game Data")

week_9_game_data <- week_9_game_data %>% 
  mutate(week = "Week 9 Game Data")

week_8_game_data <- week_8_game_data %>% 
  mutate(week = "Week 8 Game Data")

week_7_game_data <- week_7_game_data %>% 
  mutate(week = "Week 7 Game Data")

week_6_game_data <- week_6_game_data %>% 
  mutate(week = "Week 6 Game Data")

week_5_game_data <- week_5_game_data %>% 
  mutate(week = "Week 5 Game Data")

week_4_game_data <- week_4_game_data %>% 
  mutate(week = "Week 4 Game Data")

week_3_game_data <- week_3_game_data %>% 
  mutate(week = "Week 3 Game Data")

week_2_game_data <- week_2_game_data %>% 
  mutate(week = "Week 2 Game Data")

week_1_game_data <- week_1_game_data %>% 
  mutate(week = "Week 1 Game Data")

cumulative_game_data <- cumulative_game_data %>% 
  mutate(week = "Cumulative Game Data")



combined <- rbind(week_1_game_data, week_2_game_data, week_3_game_data, week_4_game_data, week_5_game_data, 
                  week_6_game_data, week_7_game_data, week_8_game_data, week_9_game_data, 
                  week_10_game_data, week_11_game_data, week_12_game_data, week_13_game_data,
                  cumulative_game_data)


##Section 2 ____________________________________________________
#set up the user interface
ui <- navbarPage("Women's Basketball Player Comparison 2", theme = shinytheme("darkly"),
                 
                 tabPanel("Player Comparison", 
                          
                          fluidPage( #allows layout to fill browser window
                            titlePanel(h1("WBB Player Comparison", align = "center")),
                            #adds a title to page and browser tab
                            #-use "title = 'tab name'" to name browser tab
                            sidebarPanel( #designates location of following items
                              htmlOutput("data_selector"),#add selectinput boxs
                              htmlOutput("player_selector1"),  # from objects created in server
                              htmlOutput("player_selector2")# from objects created in server
                            ),
                            
                            mainPanel(
                              plotOutput("plot1"), #put plot item in main area
                              tableOutput("player_table")
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
                selected = unique(data_available)[1])
  })
  output$player_selector2 = renderUI({#creates player select box object called in ui
    
    data_available = combined[combined$week == input$data_set, c("Name", "BPM_Pred")]
    #creates a reactive list of available players based on the dataset selection made
    
    selectInput(inputId = "player2", #name of input
                label = "Player 2:", #label displayed in ui
                choices = unique(data_available[1]), #calls list of available counties
                selected = unique(data_available)[1])
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
})#close the shinyServer

##Section 4____________________________________________________
shinyApp(ui = ui, server = server) #need this if combining ui and server into one file.

