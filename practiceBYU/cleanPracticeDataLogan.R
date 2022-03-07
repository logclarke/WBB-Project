library(readxl)
# assertthat::assert_that(getwd()=="~/Desktop/curr_Chris/Sportsdada/WBB-Project/practiceBYU")

#setwd("/Users/loganclarke/Documents/WBB Project")
# totals <- read_xlsx(file.path("..", "Practice Data", "practiceDataRaw.xlsx"), 
#                     sheet = "TOTALS & AVERAGES")

totals <- read_xlsx(path = "/Users/loganclarke/WBB R Project/Practice Data/practiceDataRaw.xlsx", 
                    sheet = "TOTALS & AVERAGES")

totals <- totals[5:which(totals$...3 == "TEAM")[1],]

for (i in 1:length(totals$'BASKETBALL  PLAYING  STATISTICS')) {
  if ((i %% 2) == 0) {
    if (!is.na(totals$...2[i])) {
      totals$...2[i-1] <- paste(totals$...2[i-1], totals$...2[i])
    }
  }
}

totals <- totals[seq(1, length(totals$'BASKETBALL  PLAYING  STATISTICS'), 2),]

totals$...2[length(totals$...2)] <- "TEAM"

totals <- totals[-which(is.na(totals$...2)),]

totals <- totals[-1,-c(3, 4, 6, 8, seq(11, 21, 2), 25:31, seq(33, 61, 2))]


colnames(totals) <- c("No.", "Name", "Games", "Starts", "Fouls", "DREB", "OREB", 
                      "TREB", "TOV", "Steals", "TOVminSTL", "Blocks", "Assists", 
                      "Charges", "TWOpa", "TWOpm", "TWOper", "THREEpa", "THREEpm", 
                      "THREEper", "FGa", "FGm", "FGper", "FTa", "FTm", "FTper", 
                      "TOTa", "TOTm", "TOTper", "Points")

write.csv(totals, 
          "/Users/loganclarke/WBB R Project/Practice Data/practiceDataClean.csv", 
          row.names = FALSE)


#note on 07 march i had to add info for kyra beckman and kayla belles-lee
team <- read.csv("https://raw.githubusercontent.com/logclarke/WBB-Project/main/practiceBYU/team_info.csv")

totals <- cbind(team[,2:length(team)], totals[-length(totals$Name),])

totals <- totals[,c("Name", "Age", "Pos", "Ft", "Inches", "Exp", "Games", "Starts", "FGm", 
                    "FGa", "FGper", "THREEpm", "THREEpa", "THREEper", "TWOpm", "TWOpa", "TWOper", 
                    "FTm", "FTa", "FTper", "OREB", "DREB", "TREB", "Assists", "Steals", "Blocks", 
                    "TOV", "Fouls", "Points")]

# totals <- totals[-13, ]

write.csv(totals, 
          "/Users/loganclarke/WBB R Project/Practice Data/practiceBoxScore.csv", 
          row.names = FALSE)









