library(gbm)
library(dplyr)

box <- read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/modelBuild/CompiledBox.csv")

hist(box$BPM)
hist(as.numeric(box$FT.))


# Get rid of the big outliers
plot(box$BPM)
box$X2P. <- as.numeric(box$X2P.)
box$FT. <- as.numeric(box$FT.)
box$X3P. <- as.numeric(box$X3P.)
box$FG. <- as.numeric(box$FG.)


box <- box[-1,]
box <- box[!is.na(box$X2P.),]
box <- box[!is.na(box$FT.),]
box <- box[!is.na(box$X3P.),]


box <- box[box$BPM > -15,]

gbm_1 <- gbm(BPM~ FG  + FGA + X3P + X3PA + X2P + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS + FG. + 
               X3P.,
             data=box, distribution = "gaussian", n.trees = 3000, interaction.depth = 10, shrinkage = 0.01)  

week1 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_1_game_data.csv")
library(stringr)
# Get rid of the number
week1$Name = paste(str_split(week1$Name, ' ', simplify = TRUE)[,2], str_split(week1$Name, ' ', simplify = TRUE)[,3])

head(week1)
pweek1 = predict(gbm_1, newdata = week1)
dfweek1 <- data.frame("Name" = week1$Name, "BPM_Pred" = pweek1)
#rounding makes it possible to filter
dfweek1$BPM_Pred <- round(dfweek1$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek1 <- dfweek1 %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(dfweek1 %>% arrange(-BPM_Pred), caption = "Week 1 games player rankings")



# Week 2

week2 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_2_game_data.csv")
library(stringr)
# Get rid of the number
week2$Name = paste(str_split(week2$Name, ' ', simplify = TRUE)[,2], str_split(week2$Name, ' ', simplify = TRUE)[,3])
pweek2 = predict(gbm_1, newdata = week2)
dfweek2 <- data.frame("Name" = week2$Name, "BPM_Pred" = pweek2)
#rounding makes it possible to filter
dfweek2$BPM_Pred <- round(dfweek2$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek2 <- dfweek2 %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(dfweek2 %>% arrange(-BPM_Pred), caption = "Week 2 games player rankings")


# Week 3
week3 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_3_game_data.csv")
library(stringr)
# Get rid of the number
week3$Name = paste(str_split(week3$Name, ' ', simplify = TRUE)[,2], str_split(week3$Name, ' ', simplify = TRUE)[,3])
pweek3 = predict(gbm_1, newdata = week3)
dfweek3 <- data.frame("Name" = week3$Name, "BPM_Pred" = pweek3)
#rounding makes it possible to filter
dfweek3$BPM_Pred <- round(dfweek3$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek3 <- dfweek3 %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(dfweek3 %>% arrange(-BPM_Pred), caption = "Week 3 games player rankings")



# Week4
week4 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_4_game_data.csv")
library(stringr)
# Get rid of the number
week4$Name = paste(str_split(week4$Name, ' ', simplify = TRUE)[,2], str_split(week4$Name, ' ', simplify = TRUE)[,3])
pweek4 = predict(gbm_1, newdata = week4)
dfweek4 <- data.frame("Name" = week4$Name, "BPM_Pred" = pweek4)
#rounding makes it possible to filter
dfweek4$BPM_Pred <- round(dfweek4$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek4 <- dfweek4 %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(dfweek4 %>% arrange(-BPM_Pred), caption = "Week 4 games player rankings")


# Week 5

week5 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_5_game_data.csv")
library(stringr)
# Get rid of the number
week5$Name = paste(str_split(week5$Name, ' ', simplify = TRUE)[,2], str_split(week5$Name, ' ', simplify = TRUE)[,3])
pweek5 = predict(gbm_1, newdata = week5)
dfweek5 <- data.frame("Name" = week5$Name, "BPM_Pred" = pweek5)
#rounding makes it possible to filter
dfweek5$BPM_Pred <- round(dfweek5$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek5 <- dfweek5 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek5 %>% arrange(-BPM_Pred), caption = "Week 5 games player rankings")

# Week 6

week6 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_6_game_data.csv")
library(stringr)
# Get rid of the number
week6$Name = paste(str_split(week6$Name, ' ', simplify = TRUE)[,2], str_split(week6$Name, ' ', simplify = TRUE)[,3])
pweek6 = predict(gbm_1, newdata = week6)
dfweek6 <- data.frame("Name" = week6$Name, "BPM_Pred" = pweek6)
#rounding makes it possible to filter
dfweek6$BPM_Pred <- round(dfweek6$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek6 <- dfweek6 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek6 %>% arrange(-BPM_Pred), caption = "Week 6 games player rankings")


# Week 7

week7 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_7_game_data.csv")
library(stringr)
# Get rid of the number
week7$Name = paste(str_split(week7$Name, ' ', simplify = TRUE)[,2], str_split(week7$Name, ' ', simplify = TRUE)[,3])
pweek7 = predict(gbm_1, newdata = week7)
dfweek7 <- data.frame("Name" = week7$Name, "BPM_Pred" = pweek7)
#rounding makes it possible to filter
dfweek7$BPM_Pred <- round(dfweek7$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek7 <- dfweek7 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek7 %>% arrange(-BPM_Pred), caption = "Week 7 games player rankings")


# Week 8

week8 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_8_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week8$Name = paste(str_split(week8$Name, ' ', simplify = TRUE)[,2], str_split(week8$Name, ' ', simplify = TRUE)[,3])
pweek8 = predict(gbm_1, newdata = week8)
dfweek8 <- data.frame("Name" = week8$Name, "BPM_Pred" = pweek8)
#rounding makes it possible to filter
dfweek8$BPM_Pred <- round(dfweek8$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek8 <- dfweek8 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek8 %>% arrange(-BPM_Pred), caption = "Week 8 games player rankings")


# Week 9

week9 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_9_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week9$Name = paste(str_split(week9$Name, ' ', simplify = TRUE)[,2], str_split(week9$Name, ' ', simplify = TRUE)[,3])
pweek9 = predict(gbm_1, newdata = week9)
dfweek9 <- data.frame("Name" = week9$Name, "BPM_Pred" = pweek9)
#rounding makes it possible to filter
dfweek9$BPM_Pred <- round(dfweek9$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek9 <- dfweek9 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek9 %>% arrange(-BPM_Pred), caption = "Week 9 games player rankings")


# Week 10

week10 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_10_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week10$Name = paste(str_split(week10$Name, ' ', simplify = TRUE)[,2], str_split(week10$Name, ' ', simplify = TRUE)[,3])
pweek10 = predict(gbm_1, newdata = week10)
dfweek10 <- data.frame("Name" = week10$Name, "BPM_Pred" = pweek10)
#rounding makes it possible to filter
dfweek10$BPM_Pred <- round(dfweek10$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek10 <- dfweek10 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek10 %>% arrange(-BPM_Pred), caption = "Week 10 games player rankings")


# Week 11

week11 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_11_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week11$Name = paste(str_split(week11$Name, ' ', simplify = TRUE)[,2], str_split(week11$Name, ' ', simplify = TRUE)[,3])
pweek11 = predict(gbm_1, newdata = week11)
dfweek11 <- data.frame("Name" = week11$Name, "BPM_Pred" = pweek11)
#rounding makes it possible to filter
dfweek11$BPM_Pred <- round(dfweek11$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek11 <- dfweek11 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek11 %>% arrange(-BPM_Pred), caption = "Week 11 games player rankings")


# Week 12

week12 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_12_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week12$Name = paste(str_split(week12$Name, ' ', simplify = TRUE)[,2], str_split(week12$Name, ' ', simplify = TRUE)[,3])
pweek12 = predict(gbm_1, newdata = week12)
dfweek12 <- data.frame("Name" = week12$Name, "BPM_Pred" = pweek12)
#rounding makes it possible to filter
dfweek12$BPM_Pred <- round(dfweek12$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek12 <- dfweek12 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek12 %>% arrange(-BPM_Pred), caption = "Week 12 games player rankings")


# Week 13

week13 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_13_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week13$Name = paste(str_split(week13$Name, ' ', simplify = TRUE)[,2], str_split(week13$Name, ' ', simplify = TRUE)[,3])
pweek13 = predict(gbm_1, newdata = week13)
dfweek13 <- data.frame("Name" = week13$Name, "BPM_Pred" = pweek13)
#rounding makes it possible to filter
dfweek13$BPM_Pred <- round(dfweek13$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek13 <- dfweek13 %>% 
  filter(BPM_Pred != -6.69274)
knitr::kable(dfweek13 %>% arrange(-BPM_Pred), caption = "Week 13 games player rankings")


# Week 14

week14 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_14_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week14$Name = paste(str_split(week14$Name, ' ', simplify = TRUE)[,2], str_split(week14$Name, ' ', simplify = TRUE)[,3])
pweek14 = predict(gbm_1, newdata = week14)
dfweek14 <- data.frame("Name" = week14$Name, "BPM_Pred" = pweek14)
#rounding makes it possible to filter
dfweek14$BPM_Pred <- round(dfweek14$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek14 <- dfweek14 %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(dfweek14 %>% arrange(-BPM_Pred), caption = "Week 14 games player rankings")


# Week 15

week15 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_15_game_data.csv")
library(stringr)
library(gbm)
library(dplyr)
# Get rid of the number
week15$Name = paste(str_split(week15$Name, ' ', simplify = TRUE)[,2], str_split(week15$Name, ' ', simplify = TRUE)[,3])
pweek15 = predict(gbm_1, newdata = week15)
dfweek15 <- data.frame("Name" = week15$Name, "BPM_Pred" = pweek15)
#rounding makes it possible to filter
dfweek15$BPM_Pred <- round(dfweek15$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
dfweek15 <- dfweek15 %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(dfweek15 %>% arrange(-BPM_Pred), caption = "Week 15 games player rankings")


# Cumulative
#update the date every week on the csv file that's being read in
cum_games = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/cum_total_28_feb.csv")
cum_games$Name = paste(str_split(cum_games$Name, ' ', simplify = TRUE)[,2], str_split(cum_games$Name, ' ', simplify = TRUE)[,3])
pcum = predict(gbm_1, newdata = cum_games)
df_cum <- data.frame("Name" = cum_games$Name, "BPM_Pred" = pcum)
#rounding makes it possible to filter
df_cum$BPM_Pred <- round(df_cum$BPM_Pred, 5)
#this gets rid of anyone who didn't play in games
df_cum <- df_cum %>% 
  filter(BPM_Pred != -6.67132)
knitr::kable(df_cum %>% arrange(-BPM_Pred), caption = "All games player rankings")

