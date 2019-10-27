#NBA DFS Optimization

setwd("C:/Users/gelmashni/Dropbox/Northwestern/NBA Hackathon/DFS")

devtools::install_github("zamorarr/coach")
library(coach)

require(xlsx)
data <- read.xlsx(file="NBADaily.xlsx",sheetName = "Oct26FD")
print(data)

library(plyr)

count(data, "player")

str(data)
head(data)
names(data)
summary(data)

data$row_id <- as.integer(data$row_id)
data$player_id <- as.character(data$player_id)
data$player <- as.character(data$player)
data$team <- as.character(data$team)
data$position <- as.character(data$position)
data$salary <- as.integer(data$salary)

set.seed(100)
n <- nrow(data)
data$fpts_proj <- data$fpts_avg
data$fpts_proj <- rnorm(n, data$fpts_avg)
print(data)

model <- model_fd_nba(data)

scores <- optimize_generic(data, model, L = 20)
scores

write.csv(scores, file = "NBA_Predictions.csv")
