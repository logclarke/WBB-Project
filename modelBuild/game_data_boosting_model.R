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
knitr::kable(dfweek1 %>% arrange(-BPM_Pred), caption = "Week 1 games player rankings")



# Week 2

week2 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_2_game_data.csv")
library(stringr)
# Get rid of the number
week2$Name = paste(str_split(week2$Name, ' ', simplify = TRUE)[,2], str_split(week2$Name, ' ', simplify = TRUE)[,3])
pweek2 = predict(gbm_1, newdata = week2)
dfweek2 <- data.frame("Name" = week2$Name, "BPM_Pred" = pweek2)
knitr::kable(dfweek2 %>% arrange(-BPM_Pred), caption = "Week 2 games player rankings")


# Week 3
week3 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_3_game_data.csv")
library(stringr)
# Get rid of the number
week3$Name = paste(str_split(week3$Name, ' ', simplify = TRUE)[,2], str_split(week3$Name, ' ', simplify = TRUE)[,3])
pweek3 = predict(gbm_1, newdata = week3)
dfweek3 <- data.frame("Name" = week3$Name, "BPM_Pred" = pweek3)
knitr::kable(dfweek3 %>% arrange(-BPM_Pred), caption = "Week 3 games player rankings")



# Week4
week4 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_4_game_data.csv")
library(stringr)
# Get rid of the number
week4$Name = paste(str_split(week4$Name, ' ', simplify = TRUE)[,2], str_split(week4$Name, ' ', simplify = TRUE)[,3])
pweek4 = predict(gbm_1, newdata = week4)
dfweek4 <- data.frame("Name" = week4$Name, "BPM_Pred" = pweek4)
knitr::kable(dfweek4 %>% arrange(-BPM_Pred), caption = "Week 4 games player rankings")


# Week 5

week5 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_5_game_data.csv")
library(stringr)
# Get rid of the number
week5$Name = paste(str_split(week5$Name, ' ', simplify = TRUE)[,2], str_split(week5$Name, ' ', simplify = TRUE)[,3])
pweek5 = predict(gbm_1, newdata = week5)
dfweek5 <- data.frame("Name" = week5$Name, "BPM_Pred" = pweek5)
knitr::kable(dfweek5 %>% arrange(-BPM_Pred), caption = "Week 5 games player rankings")

# Week 6

week6 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_6_game_data.csv")
library(stringr)
# Get rid of the number
week6$Name = paste(str_split(week6$Name, ' ', simplify = TRUE)[,2], str_split(week6$Name, ' ', simplify = TRUE)[,3])
pweek6 = predict(gbm_1, newdata = week6)
dfweek6 <- data.frame("Name" = week6$Name, "BPM_Pred" = pweek6)
knitr::kable(dfweek6 %>% arrange(-BPM_Pred), caption = "Week 6 games player rankings")


# Week 7

week7 = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/week_7_game_data.csv")
library(stringr)
# Get rid of the number
week7$Name = paste(str_split(week7$Name, ' ', simplify = TRUE)[,2], str_split(week7$Name, ' ', simplify = TRUE)[,3])
pweek7 = predict(gbm_1, newdata = week7)
dfweek7 <- data.frame("Name" = week7$Name, "BPM_Pred" = pweek7)
knitr::kable(dfweek7 %>% arrange(-BPM_Pred), caption = "Week 7 games player rankings")


# Cumulative
cum_games = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/Game-data/cum_total_27_dec.csv")
cum_games$Name = paste(str_split(cum_games$Name, ' ', simplify = TRUE)[,2], str_split(cum_games$Name, ' ', simplify = TRUE)[,3])
pcum = predict(gbm_1, newdata = cum_games)
df_cum <- data.frame("Name" = cum_games$Name, "BPM_Pred" = pcum)
knitr::kable(df_cum %>% arrange(-BPM_Pred), caption = "All games player rankings")

