library(dplyr)
practice <- read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/practiceBYU/practiceDataClean.csv")
colnames(practice)
practice <- practice[1:18,]

names(practice) <- c("No.", "Name", "Games", "Starts", "Fouls", "DRB", "ORB", "TRB", "TOV", "STL", "TOVminSTL", "BLK", "AST", "Charges",  "X2PA", "X2P", "X2P.", 
                     "X3PA", "X3P", "X3P.", "FGA", "FG", "FG.", "FTA", "FT", "FT.", "Tota", "Totm", "TOTper", "PTS")

# length(colnames(practice[,c(6:10,12:13,15:26,30)]))
# length(colnames(box[,c(7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28)]))
# library(BART)
# bart = gbart(x.train = box[,c(7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28)],
#              y.train = box$BPM, 
#              x.test = practice[,c(6:10,12:13,15:26,30)], 
#              ntree = n.trees[2], ndpost=1200, nskip = 300, sparse = FALSE)
# 
# df_bart <- data.frame("Name" = practice$Name, "Prediction" = as.numeric(bart$yhat.test.mean))
# df_bart = df_bart %>% arrange(-Prediction)
# bart$varcount.mean

box <- read.csv("CompiledBox.csv")

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


# GBM Practice data
library(gbm)
gbm1 <- gbm(BPM~ FG  + FGA + X3P + X3PA + X2P + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS + FG. + 
              X3P.,
            data=box, distribution = "gaussian", n.trees = 3000, interaction.depth = 10, shrinkage = 0.01)  
p = predict(gbm1, newdata = practice)
df <- data.frame("Name" = practice$Name, "BPM_Pred" = p)
knitr::kable(df %>% arrange(-BPM_Pred))

df %>% arrange(-BPM_Pred)
library(vip)
vip(gbm1)

knitr::kable(df_bart)

practice %>% filter

