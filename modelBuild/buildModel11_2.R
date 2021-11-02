

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

hist(box$BPM)

hist(box$BPM)


# Bagging
library(ranger)

bag <- ranger(BPM~ FG  + FG + FGA + X3P + X3PA + X2P. + X2PA + FG. + FT. + X3P. + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS + eFG.,
              data=box, num.trees = 1000, mtry = 14, importance = "permutation") 
summary(bag)

sqrt(bag$prediction.error)

library(ggplot2)
library(vip)
vip(bag)

# Random Forest model

bag <- ranger(BPM~  FG + FGA + X3P + X3PA + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=box, num.trees = 1000, mtry = 4, importance = "permutation") 

sqrt(bag$prediction.error)



library(caret)
library(ellipsis)


## BOOSTING


### Boosting
library(gbm)
gbm1 <- gbm(BPM~ FG  + FG + FGA + X3P + X3PA + X2P. + X2PA + FG. + FT. + X3P. + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
            data = box, 
            distribution = "gaussian", n.trees = 2000, interaction.depth = 3, shrinkage = 0.01, train.fraction = 0.8, 
            cv.folds = 10)  


# Check performance using the out-of-bag (OOB) error; the OOB error typically
# underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

# Check performance using the 20% heldout test set
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1, method = "cv")
print(best.iter)


### Identify test sets
library(magrittr)
K = 5
possibilities = 1:nrow(box)
this.many = round(nrow(box)/K)

splits <- list()
already.used <- NA
for(i in 2:K){
  samp <- sample(possibilities, this.many, replace = FALSE)
  splits[[i]] <- samp
  possibilities = possibilities[!(possibilities %in% splits[[i]]) ]
}
splits[[1]] = possibilities


SSE <-SumE <- 0
oob.mse.gbm <- NA
boost.rsq <- NA

###
#Gradient Boosting Model
###

SSE = 0
for(i in 1:5){
  train.data = box[-splits[[i]],]
  test.data = box[splits[[i]], ]
  
  gbm1 <- gbm(BPM~ FG  + FGA + X3P + X3PA + X2P + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS + FG. + 
                X3P.,
              data=train.data, distribution = "gaussian", n.trees = 3000, interaction.depth = 10, shrinkage = 0.01)  
  p = predict(gbm1, newdata = test.data)
  
  oob.mse.gbm[i] <- (p- test.data$BPM)^2 %>% mean()

  SSE = SSE + sum((p - test.data$BPM)^2)
  SumE = SumE + sum(p - test.data$BPM)
}



plot(test.data$BPM, p)


sqrt(mean(oob.mse.gbm))
mse = (SSE/nrow(box))
sqrt(mse)
bias = SumE/nrow(box)
rsq = 1- SSE/(sum((box$BPM - mean(box$BPM))^2))
rsq

sd(box$BPM)

####
## BART
####

library(BART)
library(magrittr)
SSE = 0
n.trees <- seq(20, 300, length.out = 5)
oob.mse <- NA
#120 trees i think is my best
for(i in 1:5){
  
  train <- box[-splits[[i]],]
  test <- box[splits[[i]],]
  bart = gbart(x.train = train[,c(7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28)],
               y.train = train$BPM, 
               x.test = test[,c(7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28)], 
               ntree = n.trees[2], ndpost=1200, nskip = 300, sparse = FALSE)
  
  oob.mse[i] <- (bart$yhat.test.mean - test$BPM)^2 %>% mean()
  SSE = SSE + sum((bart$yhat.test.mean - test$BPM)^2)

  
}

colnames(train[,c( 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28)])

sqrt(mean(oob.mse))
1-SSE/sum((box$BPM - mean(box$BPM))^2)

plot(test$BPM, bart$yhat.test.mean)

bart.vip <- as.data.frame(sort(bart$varcount.mean, decreasing = TRUE))  
(bart.vip)
dim(bart.vip)
names(bart$varcount.mean)
df <- data.frame("Variable" = names(bart$varcount.mean), "Importance" = bart$varcount.mean)
df1 = df %>% arrange(-Importance)

# Variable Importance Plot
ggplot(data = df1, mapping = aes(y = reorder(Variable, Importance), x = Importance)) + 
  geom_col() + labs(title = "Variable Importance according to BART model", y = "")



