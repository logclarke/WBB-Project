transfer = read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/transferModel/full_stats.csv")

transfer %>% View()



library(dplyr)

# transfer_ = transfer %>% mutate(PTS = PPG*GP) %>% mutate(TRB = REBPG*GP) %>% mutate(AST = ASTPG*GP) %>% 
#   mutate(STL = STLPG*GP) %>% mutate(BLK = BLKPG*GP) %>% mutate(TOV = TOPG*GP) %>% mutate(ORB = OR*GP) %>% 
#   mutate(DRB = DR*GP) %>% mutate(FG = FGM) %>% mutate(X3P = X3PM) %>% mutate(FT = FTM)

colnames(transfer)

transfer_ = transfer %>% mutate(PTS = PTS) %>% mutate(TRB = TREB) %>% mutate(AST = AST) %>% 
  mutate(STL = STL) %>% mutate(BLK = BLK) %>% mutate(TOV = TO) %>% mutate(ORB = OR) %>% 
  mutate(DRB = DR) %>% mutate(FG = FGM) %>% mutate(X3P = X3PM) %>% mutate(FT = FTM)

colnames(transfer_)



box <- read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/CompiledBox.csv")

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
gbm1 <- gbm(BPM ~ FG  + FGA + X3P + X3PA  + FT + FTA + ORB + DRB + TRB + AST + STL + BLK + TOV + PTS + FG. + 
              X3P.,
            data=box, distribution = "gaussian", n.trees = 3000, interaction.depth = 10, shrinkage = 0.01) 

p = predict(gbm1, newdata = transfer_)
df <- data.frame("Name" = transfer$Name, "BPM_Pred" = p)
knitr::kable(df %>% arrange(-BPM_Pred))


df %>% arrange(-BPM_Pred) %>% View()


df %>% arrange(-BPM_Pred) %>% write.csv("Womens_NCAA_Rankings.csv")

df %>% arrange(-BPM_Pred) %>% mutate(Percentile = round(percent_rank(BPM_Pred),2)) %>% write.csv("Womens_NCAA_Rankings1.csv")
library(vip)
vip(gbm1)

knitr::kable(df_bart)

practice %>% filter
