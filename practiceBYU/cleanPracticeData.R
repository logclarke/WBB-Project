library(readxl)

totals <- read_xlsx(file.path("..", "practiceBYU", "practiceDataRaw.xlsx"), 
                    sheet = "TOTALS & AVERAGES")

totals <- totals[5:which(totals$...3 == "TEAM")[1],]

for (i in 1:length(totals$`BASKETBALL  PLAYING  STATISTICS`)) {
  if ((i %% 2) == 0) {
    if (!is.na(totals$...2[i])) {
      totals$...2[i-1] <- paste(totals$...2[i-1], totals$...2[i])
    }
  }
}

totals <- totals[seq(1, length(totals$`BASKETBALL  PLAYING  STATISTICS`), 2),]

totals$...2[length(totals$...2)] <- "TEAM"

totals <- totals[-which(is.na(totals$...2)),]

totals <- totals[-1,-c(3, 4, 6, 8, seq(11, 21, 2), 25:31, seq(33, 61, 2))]


colnames(totals) <- c("No.", "Name", "Games", "Starts", "Fouls", "DREB", "OREB", 
                      "TREB", "TOV", "Steals", "TOVminSTL", "Blocks", "Assists", 
                      "Charges", "TWOpa", "TWOpm", "TWOper", "THREEpa", "THREEpm", 
                      "THREEper", "FGa", "FGm", "FGper", "FTa", "FTm", "FTper", 
                      "TOTa", "TOTm", "TOTper", "Points")

write.csv(totals, file.path("..", "practiceBYU", "practiceDataClean.csv"), row.names = FALSE)

team <- read.csv(file.path("..", "practiceBYU", "team_info.csv"))

totals <- cbind(team, totals[-length(totals$Name),])

totals <- totals[,c(1:6, 9, 10, 27, 26, 28, 24, 23, 25, 21, 20, 22, 30, 29, 31, 12, 11, 13, 18, 15, 17, 14, 35)]

write.csv(totals, file.path("..", "practiceBYU", "practiceBoxScore.csv"), row.names = FALSE)
























