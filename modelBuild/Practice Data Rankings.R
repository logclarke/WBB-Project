practice <- read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/practiceBYU/practiceBoxScore.csv")
colnames(practice)


names(practice) <- c("Name", "Age", "Pos", "Ft", "Inches", "Exp", "G", "Starts", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.",  "X2P", "X2PA", "X2P.", "FT", 
                     "FTA", "FT.", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "Fouls", "PTS")


library(BART)
bart = gbart(x.train = box[,c(7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28)],
             y.train = box$BPM, 
             x.test = practice[,c(9:27,29)], 
             ntree = n.trees[2], ndpost=1200, nskip = 300, sparse = FALSE)

df_bart <- data.frame("Name" = practice$Name, "Prediction" = as.numeric(bart$yhat.test.mean))
df_bart = df_bart %>% arrange(-Prediction)



# GBM Practice data
gbm1 <- gbm(BPM~ FG  + FGA + X3P + X3PA + X2P + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS + FG. + 
              X3P.,
            data=box, distribution = "gaussian", n.trees = 3000, interaction.depth = 10, shrinkage = 0.01)  
p = predict(gbm1, newdata = practice)
df <- data.frame("Name" = practice$Name, "BPM_Pred" = p)

df %>% arrange(-BPM_Pred)
vip(gbm1)

knitr::kable(df_bart)

