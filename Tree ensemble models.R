

box <- read.csv("CompiledBox.csv")

hist(box$BPM)
hist(as.numeric(box$FT.))

# Get rid of the big outliers
plot(box$BPM)
box <- box[-1,]
box <- box[!is.na(box$X2P.),]
box <- box[box$BPM > -10,]

hist(box$BPM)




sd(box$BPM)

# Bagging
library(ranger)

bag <- ranger(BPM~ FG  + FG + FGA + X3P + X3PA + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=box, num.trees = 100, mtry = 14, importance = "permutation") 
summary(bag)

sqrt(bag$prediction.error)

library(ggplot2)
library(vip)
vip(bag)

# Random Forest model

bag <- ranger(BPM~  FG + FGA + X3P + X3PA + X2P. + X2PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS,
              data=box, num.trees = 1000, mtry = 4, importance = "permutation") 

sqrt(bag$prediction.error)

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
sqrt(mse)
bias = SumE/nrow(box)
rsq = 1- SSE/(sum((box$BPM - mean(box$BPM))^2))




####
## BART
####

library(BART)
library(magrittr)

n.trees <- seq(20, 300, length.out = 5)
oob.mse <- NA
#120 trees i think is my best
for(i in 1:5){
  
  train <- box[-splits[[i]],]
  test <- box[splits[[i]],]
  bart = gbart(x.train = train[,c(4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 17, 18, 20, 21, 22, 23, 24, 25, 26, 28)],
               y.train = train$BPM, 
               x.test = test[,c(4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 17, 18, 20, 21, 22, 23, 24, 25, 26, 28)], 
               ntree = 30, ndpost=1200, nskip = 300, sparse = TRUE)
  
  oob.mse[i] <- (bart$yhat.test.mean - test$BPM)^2 %>% mean()

  
}

sqrt(mean(oob.mse))


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



# Couldn't get neural net package to work, I will try again with tensorflow
# ## Try a neural net
# 
# library(neuralnet)
# 
# relu = function(x) {ifelse(x<0,0*x,x)}
# relu_diff <- function(x) {x/(1+exp(-2*10*x))}
# 
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# box$BPM = range01(box$BPM)
# box$PTS = range01(box$PTS)
# box$AST = range01(box$AST)
# box$FGA = range01(box$FGA)
# box$FG = range01(box$FG)
# box$FG. = range01(box$FG.)
# box$X3P = range01(box$X3P)
# box$X3PA = range01(box$X3PA)
# box$FG = range01(box$FG)
# 
# 
# nn=neuralnet(BPM~PTS + FG + AST,
#              data=box, hidden=c(4),act.fct = relu_diff)
# 
# 
# rmse_1 <- NA
# bias_1 <- NA
# rsq_1 <- NA
# SSE <- 0
# for(nodes3 in 1:10){
#   SSE <- SumE <- 0
#   for(i in 1:K){
#     train.data = box[-splits[[i]],]
#     test.data = box[splits[[i]], ]
#     
#     nn=neuralnet(BPM ~ PTS + FG + X3P,data=train.data, hidden=c(nodes3), act.fct = relu_diff)
#     p = predict(nn,newdata = test.data)*30
#     
#     SSE = SSE + sum((p - test.data$BPM)^2)
#     SumE = SumE + sum(p - test.data$BPM)
#     
#   }
#   
#   rmse_1[nodes3] = sqrt(SSE/nrow(box))
#   bias_1[nodes3] = SumE/nrow(box)
#   rsq_1[nodes3] = 1- SSE/(sum((box$swc - mean(box$swc))^2))
#   
# }
# 
# rmse_1
# 
# 
# 
# # fit neural network - note activation function must be differentiable for this function. 
# library(neuralnet)
# with(ag,scatter.smooth(cwsi,swc,pch=19))
# nn=neuralnet(I(swc/30)~cwsi ,data=ag, hidden=2,act.fct = relu_diff)
# 
# oos = data.frame(cwsi = seq(0,1,length.out=1000))
# p = predict(nn,newdata = oos)
# lines(oos$cwsi,p*30,col=2)
# # Will need to tweak the hidden layers, which is perhaps more art than science (and more cross-validation than anything else ;)
# 
# 
# 
# 
# 
# ####
# # My code
# 
# ag <- read.table(file = "agwater.txt", header = TRUE)
# with(ag,scatter.smooth(cwsi,swc,pch=19))
# 
# ### Identify test sets
# K = 5
# possibilities = 1:nrow(ag)
# this.many = round(nrow(ag)/K)
# 
# splits <- list()
# already.used <- NA
# for(i in 2:K){
#   samp <- sample(possibilities, this.many, replace = FALSE)
#   splits[[i]] <- samp
#   possibilities = possibilities[!(possibilities %in% splits[[i]]) ]
# }
# splits[[1]] = possibilities
# 
# 
# ### Cross validate
# library(neuralnet)
# relu = function(x) {ifelse(x<0,0*x,x)}
# relu_diff <- function(x) {x/(1+exp(-2*10*x))}
# 
# 
# SSE <- 0
# SumE <- 0
# K 
# 
# nn=neuralnet(I(swc/30) ~  ,data=train.data, hidden=c(nodes, nodes2), act.fct = relu_diff)
# 
# 
# rmse <- matrix(nrow = 5, ncol = 5)
# bias <- matrix(nrow = 5, ncol = 5)
# rsq <- matrix(nrow = 5, ncol = 5)
# 
# 
# 
# for(nodes in 1:5){
#   for(nodes2 in 1:5){
#     SSE <- SumE <- 0
#     for(i in 1:K){
#       train.data = ag[-splits[[i]],]
#       test.data = ag[splits[[i]], ]
#       
#       nn=neuralnet(I(swc/30) ~ cwsi ,data=train.data, hidden=c(nodes, nodes2), act.fct = relu_diff)
#       p = predict(nn,newdata = test.data)*30
#       
#       SSE = SSE + sum((p - test.data$swc)^2)
#       SumE = SumE + sum(p - test.data$swc)
#       
#     }
#     
#     rmse[nodes, nodes2] = sqrt(SSE/nrow(ag))
#     bias[nodes, nodes2] = SumE/nrow(ag)
#     rsq[nodes, nodes2] = 1- SSE/(sum((ag$swc - mean(ag$swc))^2))
#     
#   }
#   
# }
# 
# rmse
# 
# min(rmse, na.rm = TRUE)
# bias
# rsq
# 
# set.seed(1234)
# rmse_1 <- NA
# bias_1 <- NA
# rsq_1 <- NA
# 
# for(nodes3 in 1:10){
#   SSE <- SumE <- 0
#   for(i in 1:K){
#     train.data = ag[-splits[[i]],]
#     test.data = ag[splits[[i]], ]
#     
#     nn=neuralnet(I(swc/30) ~ cwsi ,data=train.data, hidden=c(4, 4, nodes3), act.fct = relu_diff, threshold = 0.001)
#     p = predict(nn,newdata = test.data)*30
#     
#     SSE = SSE + sum((p - test.data$swc)^2)
#     SumE = SumE + sum(p - test.data$swc)
#     
#   }
#   
#   rmse_1[nodes3] = sqrt(SSE/nrow(ag))
#   bias_1[nodes3] = SumE/nrow(ag)
#   rsq_1[nodes3] = 1- SSE/(sum((ag$swc - mean(ag$swc))^2))
#   
# }
# 
# rmse_1
# bias_1
# rsq_1
# 
# SSE <- SumE <- 0
# 
# nn=neuralnet(I(swc/30) ~ cwsi ,data=train.data, hidden=c(4, 4, 4), act.fct = relu_diff)
# p = predict(nn,newdata = test.data)*30
# 
# SSE = SSE + sum((p - test.data$swc)^2)
# SumE = SumE + sum(p - test.data$swc)
# 
# 
# set.seed(1234)
# SSE <- SumE <- 0
# 
# for(i in 1:K){
#   train.data = ag[-splits[[i]],]
#   test.data = ag[splits[[i]], ]
#   
#   nn=neuralnet(I(swc/30) ~ cwsi ,data=train.data, hidden=c(4, 4, 4), act.fct = relu_diff, threshold = 0.001)
#   p = predict(nn,newdata = test.data)*30
#   
#   SSE = SSE + sum((p - test.data$swc)^2)
#   SumE = SumE + sum(p - test.data$swc)
#   
# }
# 
# rmse <- sqrt(SSE/nrow(ag))
# rmse
# 
# 
# SSE <- SumE <- 0
# 
# for(i in 1:K){
#   train.data = ag[-splits[[i]],]
#   test.data = ag[splits[[i]], ]
#   
#   nn=neuralnet(I(swc/30) ~ cwsi ,data=train.data, hidden=c(4, 4, 4), act.fct = relu_diff)
#   p = predict(nn,newdata = test.data)*30
#   
#   SSE = SSE + sum((p - test.data$swc)^2)
#   SumE = SumE + sum(p - test.data$swc)
#   
# }
# 
