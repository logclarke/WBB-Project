

box <- read.csv("bpmCalculation.csv")

hist(box$BPM)
hist(as.numeric(box$FT.))

# Get rid of the big outlier
plot(box$BPM)
box <- box[-1,]
box <- box[!is.na(box$X2P.),]



(apply(box, 2, is.na))

# single tree
# Not sure why it's not working

set.seed(2345)
samp <- sample(1:102, 90)
train <- rivers[samp,]
test <- rivers[-samp,]
library(rpart)
library(rpart.plot)
box$ï..Name
library(rpart)
library(rpart.plot)
colnames(box)

tree <- rpart(BPM~ FG  + FG + FGA + FG. + X3P + X3PA + X2P + X2PA + X2P. + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=box,  control=list(cp=0))


summary(tree)
rpart.plot(tree)
plotcp(tree)
min.cp <- tree$cptable[which.min(tree$cptable[,'xerror']),'CP']
tree.pruned <- prune(tree, cp=min.cp)
rpart.plot(tree.pruned)
yhat.tree = predict(tree.pruned, newdata=)
mean((yhat.tree - box$BPM)^2)
summary(tree)



# Bagging
library(ranger)

bag <- ranger(BPM~ FG  + FG + FGA + X3P + X3PA + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=box, num.trees = 100, mtry = 14, importance = "permutation") 
summary(bag)
bag$predictions

sqrt(bag$prediction.error)

library(ggplot2)
library(vip)
vip(bag)

# Random Forest model

bag <- ranger(BPM~  FG + FGA + X3P + X3PA + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=box, num.trees = 100, mtry = 4, importance = "permutation") 

sqrt(rf$prediction.error)

vip(rf)


library(caret)
library(ellipsis)


## BOOSTING


### Boosting
library(gbm)
gbm1 <- gbm(BPM~ FG  + FG + FGA + X3P + X3PA + X2P + X2PA +X2P. + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
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
# What if i use it to rank instead??
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

BPM~ FG  + FG + FGA + X3P + X3PA + X2P + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS

for(i in 1:5){
  train.data = box[-splits[[i]],]
  test.data = box[splits[[i]], ]
  
  gbm1 <- gbm(BPM~ FG  + FGA + X3P + X3PA + X2P + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=train.data, distribution = "gaussian", n.trees = 1010, interaction.depth = 3, shrinkage = 0.01)  
  p = predict(gbm1, newdata = test.data)
  
  oob.mse.gbm[i] <- (p- test.data$BPM)^2 %>% mean()

  SSE = SSE + sum((p - test.data$BPM)^2)
  SumE = SumE + sum(p - test.data$BPM)
}



plot(p, test.data$BPM)


sqrt(mean(oob.mse.gbm))
mse = (SSE/nrow(box))
bias = SumE/nrow(box)
rsq = 1- SSE/(sum((box$BPM - mean(box$BPM))^2))






library(gbm)
gbm_best <- gbm(BPM~ FG  + FG + FGA + FG. + X3P + X3PA + X2P + X2PA + X2P. + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
                data=box, distribution = "gaussian", n.trees = 549, interaction.depth = 3, shrinkage = 0.01, train.fraction = .5) 

which.min(gbm_best$valid.error)

gbm_best <- gbm(BPM~ FG  + FGA + FG. + X3P + X3PA + X2P + X2PA + X2P. + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
                data=box, distribution = "gaussian", n.trees = 549, interaction.depth = 3, shrinkage = 0.01) 



####
## BART
####


library(BART) #we'll use gbart or wbart functions. (I'm pretty sure gbart just uses wbart when you put in a continuous outcome)
library(magrittr)

n.trees <- seq(20, 300, length.out = 5)
oob.mse <- NA
bart.rsq <- NA
#120 trees i think is my best
for(i in 1:5){
  
  train <- box[-splits[[i]],]
  test <- box[splits[[i]],]
  bart = gbart(x.train = train[,c(4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 17, 18, 20, 21, 22, 23, 24, 25, 26, 28)],
               y.train = train$BPM, 
               x.test = test[,c(4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 17, 18, 20, 21, 22, 23, 24, 25, 26, 28)], 
               ntree = 30, ndpost=1200, nskip = 300, sparse = TRUE)
  
  oob.mse[i] <- (bart$yhat.test.mean - test$BPM)^2 %>% mean()
  bart.rsq[i] <- 1-(sum((bart$yhat.train.mean - train$BPM)^2)/sum((train$BPM - mean(train$BPM))^2))
  
  
}

sqrt(mean(oob.mse))
mean(bart.rsq)


bart.vip <- as.data.frame(sort(bart$varcount.mean, decreasing = TRUE))  
bart.vip  










hist(box$BPM)
box <- box[box$BPM > -10,]


# What if i use it to rank instead??
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

BPM~ FG  + FG + FGA + X3P + X3PA + X2P + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS

for(i in 1:5){
  train.data = box[-splits[[i]],]
  test.data = box[splits[[i]], ]
  
  gbm1 <- gbm(BPM~ FG  + FG + FGA + X3P + X3PA + X2P + X2PA + X2P.  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=train.data, distribution = "gaussian", n.trees = 549, interaction.depth = 3, shrinkage = 0.01)  
  p = predict(gbm1, newdata = test.data)
  
  p_order <- data.frame("test_BPM" = sort(p, decreasing = TRUE), "rank" = 1:nrow(test.data))
  
  oob.mse.gbm[i] <- (p- test.data$BPM)^2 %>% mean()
  
  SSE = SSE + sum((p - test.data$BPM)^2)
  SumE = SumE + sum(p - test.data$BPM)
}

library(dplyr)

ordered_rank <- test.data %>% select("Player.1", "BPM") %>% arrange(BPM)
ordered_rank$test_rank <- 1:nrow(ordered_rank)

p_order = data.frame("pred" = p, "Player.1" = test.data$Player.1) %>% arrange(pred)
p_order$pred_rank <- 1:nrow(ordered_rank)

merge_rank <- merge(p_order, ordered_rank, by = "Player.1")

merge_rank %>% arrange(pred_rank)

plot(merge_rank$pred_rank, merge_rank$test_rank)
sqrt(mean(oob.mse.gbm))


sd(box$BPM)

p_order <- data.frame("test_BPM" = sort(p, decreasing = TRUE), "rank" = 1:nrow(test.data))

test_order <- data.frame("test_BPM" = test.data$BPM, "rank" = 1:nrow(test.data))

plot(p_order$rank, test_order$rank)


mean(oob.mse.gbm)
mse = (SSE/nrow(box))
bias = SumE/nrow(box)
rsq = 1- SSE/(sum((box$BPM - mean(box$BPM))^2))



##
lm_box <- lm(BPM~ FG  + FG + FGA + X3P + X3PA + X2P + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
            data=train.data)  

summary(lm_box)
p = predict.lm(lm_box, newdata = test.data)
  