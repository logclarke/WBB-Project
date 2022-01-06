#Import libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(tm)

#Function gets rid of old name in lineup
switchWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

#Function to alphabetize lineups
alph_lineup <- function(lu){
  names <- strsplit(lu," ")
  new_order <- order(names[[1]])
  new_lineup <- ""
  for(i in 1:5){
    if(i < 5){
      new_lineup <- paste0(new_lineup, as.character(names[[1]][new_order[i]]), " ")
    }
    else{
      new_lineup <- paste0(new_lineup, as.character(names[[1]][new_order[i]]))
    }
  }
  return(new_lineup)
}

#Create a function that can get plus/minus from lineups
#Arguments: game: dataframe of raw play by play data
#byu_home: 1 if BYU is home team, 0 if away
#starters: vector of last names of BYU starters (may need to look up from ESPN)
get_lineup_pm <- function(game, byu_home, starters) {
  #Rename columns to make more sense
  colnames(game) <- c("Team", "Period", "time", "event", "away_score", "useless", "home_score")
  
  #Get rid of first row; sixth column
  game <- game[-c(1),]
  game <- game[,-c(6)]
  
  #Denote where substitutions happen
  game$is_sub <- ifelse(grepl("Player Movement", game$event, fixed = TRUE), 1, 0)
  
  #Fill in missing values (could probably be more efficient)
  i <- 1
  for(event in game$Team) {
    if(event == ""){
      game$Team[i] <- game$Team[i-1]
    }
    i <- i + 1
  }
  
  j <- 1
  for(event in game$Period) {
    if(event == ""){
      game$Period[j] <- game$Period[j-1]
    }
    j <- j + 1
  }
  
  h <- 1
  for(event in game$time) {
    if(event == ""){
      game$time[h] <- game$time[h-1]
    }
    h <- h + 1
  }
  
  #Start score at 0
  game$away_score[1] <- 0
  game$home_score[1] <- 0
  
  #Fill in score for each events
  i <- 1
  for(score in game$away_score) {
    if(score == ""){
      game$away_score[i] <- game$away_score[i-1]
    }
    i <- i + 1
  }
  
  i <- 1
  for(score in game$home_score) {
    if(is.na(score)){
      game$home_score[i] <- game$home_score[i-1]
    }
    i <- i + 1
  }
  
  #Create a column with BYUs starters
  game$byu_lineup <- ""
  game$byu_lineup <- starters
  
  #2 if/else depending on if BYU is home
  #Find substitution moments and adjust lineup
  if(byu_home == 1) {
    for(i in 1:nrow(game)){
      if(game$is_sub[i] == 1){
        if(grepl("Home Player", game$event[i], fixed = TRUE)){
          #player <- word(game$event[i], -1)
          if(grepl("Sub In", game$event[i], fixed = TRUE)){
            new_player <- word(game$event[i], -1)
            game$byu_lineup[i:nrow(game)] <- paste0(game$byu_lineup[i], " ", new_player)
          }
          if(grepl("Sub Out", game$event[i], fixed = TRUE)){
            old_player <- word(game$event[i], -1)
            new_lineup <- switchWords(game$byu_lineup[i], old_player)
            game$byu_lineup[i:nrow(game)] <- new_lineup
          }
        }
      }
    }
    #Filter to only the substitution moments and get time in minutes
    sub_windows <- game %>%
      filter(is_sub == 1) %>%
      filter(grepl("Home Player", event, fixed = TRUE))
    
    sub_windows$minutes <- (as.integer(sub(".*:", "", sub_windows$time))/60) + as.integer(sub(":.*", "", sub_windows$time))
    
    sub_windows$minutes <- ifelse(sub_windows$Period == 1, sub_windows$minutes + 30, sub_windows$minutes)
    sub_windows$minutes <- ifelse(sub_windows$Period == 2, sub_windows$minutes + 20, sub_windows$minutes)
    sub_windows$minutes <- ifelse(sub_windows$Period == 3, sub_windows$minutes + 10, sub_windows$minutes)
    
    #Put in a row at beginning and end of the game
    sub_windows <- rbind(data.frame(Team = "BYU", Period = 1, time = "10:00", event = "Start",
                                    away_score = 0, home_score = 0, is_sub = 1, byu_lineup = starters,
                                    minutes = 40.0), sub_windows)
    sub_windows <- rbind(sub_windows, data.frame(Team = "BYU", Period = 0, time = "0", event = "End",
                                                 away_score = 64, home_score = 80, is_sub = 1, byu_lineup = "",
                                                 minutes = 0.0))
    
    #Get final score and final lineup
    sub_windows$byu_lineup[nrow(sub_windows)] <- sub_windows$byu_lineup[nrow(sub_windows) - 1]
    sub_windows$away_score[nrow(sub_windows)] <- as.numeric(game$away_score[nrow(game)])
    sub_windows$home_score[nrow(sub_windows)] <- as.numeric(game$away_score[nrow(game)])
    
    #Get plus/minus
    sub_windows$plus_minus <- as.numeric(sub_windows$home_score) - as.numeric(sub_windows$away_score)
    
  }
  else{
    for(i in 1:nrow(game)){
      if(game$is_sub[i] == 1){
        if(grepl("Away Player", game$event[i], fixed = TRUE)){
          #player <- word(game$event[i], -1)
          if(grepl("Sub In", game$event[i], fixed = TRUE)){
            new_player <- word(game$event[i], -1)
            game$byu_lineup[i:nrow(game)] <- paste0(game$byu_lineup[i], " ", new_player)
          }
          if(grepl("Sub Out", game$event[i], fixed = TRUE)){
            old_player <- word(game$event[i], -1)
            new_lineup <- switchWords(game$byu_lineup[i], old_player)
            game$byu_lineup[i:nrow(game)] <- new_lineup
          }
        }
      }
    }
    #Filter to only the substitution moments and get time in minutes
    sub_windows <- game %>%
      filter(is_sub == 1) %>%
      filter(grepl("Away Player", event, fixed = TRUE))
    
    sub_windows$minutes <- (as.integer(sub(".*:", "", sub_windows$time))/60) + as.integer(sub(":.*", "", sub_windows$time))
    
    sub_windows$minutes <- ifelse(sub_windows$Period == 1, sub_windows$minutes + 30, sub_windows$minutes)
    sub_windows$minutes <- ifelse(sub_windows$Period == 2, sub_windows$minutes + 20, sub_windows$minutes)
    sub_windows$minutes <- ifelse(sub_windows$Period == 3, sub_windows$minutes + 10, sub_windows$minutes)
    
    #Put in a row at beginning and end of the game
    sub_windows <- rbind(data.frame(Team = "BYU", Period = 1, time = "10:00", event = "Start",
                                    away_score = 0, home_score = 0, is_sub = 1, byu_lineup = starters,
                                    minutes = 40.0), sub_windows)
    
    sub_windows <- rbind(sub_windows, data.frame(Team = "BYU", Period = 0, time = "0", event = "End",
                                                 away_score = 0, home_score = 0, is_sub = 1, byu_lineup = "",
                                                 minutes = 0.0))
    
    #Get final score and finish lineup
    sub_windows$byu_lineup[nrow(sub_windows)] <- sub_windows$byu_lineup[nrow(sub_windows) - 1]
    sub_windows$away_score[nrow(sub_windows)] <- as.numeric(game$away_score[nrow(game)])
    sub_windows$home_score[nrow(sub_windows)] <- as.numeric(game$home_score[nrow(game)])
    
    #Get plus/minus
    sub_windows$plus_minus <- as.numeric(sub_windows$away_score) - as.numeric(sub_windows$home_score)
    
  }
  
  #Filter to only get lineups with 5 players
  five_players <- sub_windows %>%
    filter(str_count(byu_lineup, "\\S+") == 5)
  
  #Get time played for each lineup
  five_players$lineup_minutes <- 0
  five_players$lineup_pm <- 0
  
  
  for(i in 1:(nrow(five_players) - 1)){
    five_players$lineup_minutes[i] <- five_players$minutes[i] - five_players$minutes[i+1]
    five_players$lineup_pm[i] <- five_players$plus_minus[i+1] - five_players$plus_minus[i]
  }
  
  #Alphabetize the lineups so they can be grouped together
  for(i in 1:nrow(five_players)){
    five_players$byu_lineup[i] <- alph_lineup(five_players$byu_lineup[i]) 
  }
  
  #Final cleaning of dataframe to get only output
  lineup_df <- five_players %>%
    select(byu_lineup, lineup_minutes, lineup_pm) %>%
    group_by(byu_lineup) %>%
    summarise_each(funs(sum)) %>%
    filter(lineup_minutes > 0) %>%
    arrange(desc(lineup_minutes)) %>%
    rename(lineup_plus_minus = lineup_pm)
  
  lineup_df$lineup_minutes <- round(lineup_df$lineup_minutes, 2)
  
  return(lineup_df)
}

#Use copy and pasted play by play data from synergy
#Read in data from each game
fresno <- read.csv("Game_pbp/byu_fs_wbb.csv")
libs <- read.csv("Game_pbp/libscomb.csv")
wvu <- read.csv("Game_pbp/wvu.csv")
utah <- read.csv("Game_pbp/utah.csv")
msu <- read.csv("Game_pbp/montanast.csv") #Doesnt have player movement
florida_st <- read.csv("Game_pbp/floridast.csv")
usu <- read.csv("Game_pbp/usu.csv")
asu <- read.csv("Game_pbp/asu.csv")
boise <- read.csv("Game_pbp/boise.csv")



starters <- c("Gustin Gonzales Johnson Albiero Graham")
starters2 <- c("Gustin Gonzales Harding Albiero Graham")
fresnoTest <- get_lineup_pm(fresno, 1, starters)
libsTest <- get_lineup_pm(libs, 1, starters)
wvuTest <- get_lineup_pm(wvu, 0, starters) 
msuTest <- get_lineup_pm(msu, 0, starters2) #empty
utahTest <- get_lineup_pm(utah, 0, starters2)
flaSt_test <- get_lineup_pm(florida_st, 1, starters)
usu_test <- get_lineup_pm(usu, 1, starters)
asu_test <- get_lineup_pm(asu, 1, starters)
boise_test <- get_lineup_pm(boise, 1, starters)


#Put all the games together
all_games <- rbind(fresnoTest, libsTest, wvuTest, utahTest, flaSt_test, usu_test,
      asu_test, boise_test)

totals <- all_games %>%
  group_by(byu_lineup) %>%
  summarise_each(funs(sum)) %>%
  arrange(desc(lineup_minutes))

write.csv(totals, "Game_pbp/all_games.csv")
write.csv(fresnoTest, "Game_pbp/fresno_pm.csv")
write.csv(libsTest, "Game_pbp/libscomb_pm.csv")
write.csv(wvuTest, "Game_pbp/wvu_pm.csv")
write.csv(utahTest, "Game_pbp/utah_pm.csv")
write.csv(flaSt_test, "Game_pbp/florida_st_pm.csv")
write.csv(usu_test, "Game_pbp/usu_pm.csv")
write.csv(asu_test, "Game_pbp/asu_pm.csv")
write.csv(boise_test, "Game_pbp/boise_pm.csv")