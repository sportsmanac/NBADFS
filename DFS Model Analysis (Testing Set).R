#NBA 2018-2019 Test Results

# Set working directory and upload data set
setwd("C:/Users/gelmashni/Dropbox/Sportsmanac/NBA")

require(xlsx)
data <- read.xlsx(file="NBADK1819.xlsx",sheetName = "Sheet1")

# Inspect full data set
str(data)
head(data)
names(data)
summary(data)

nbasub1 <- subset(data[which(data$Salary > 5700),])
summary(nbasub1)
nbasub2 <- subset(data[which(data$Salary < 3400),])
summary(nbasub2)
nbasubhigh <- subset(data[which(data$Salary > 9000),])
summary(nbasubhigh)
nbasubsuperhigh <- subset(data[which(data$Salary > 11000),])
summary(nbasubsuperhigh)
nbasubmid <- subset(data[which(data$Salary > 3300 & data$Salary < 5800 ),])
summary(nbasubmid)
nbasubmid <- subset(data[which(data$Salary > 11000 & data$Salary < 14000 ),])
summary(nbasubmid)

# EDA
par(mfrow=c(2,2))
hist(data$Predict, col = "navy", xlab = "DK Salary", main = "Histogram of DraftKings Salary", ylim = c(0,4000))
boxplot(data$Predict, col = "navy", main = "Boxplot of DraftKings Salary")
hist(data$Diff, col = "navy", xlab = "DK Points", main = "Histogram of DraftKings Points", ylim = c(0,4000))
boxplot(data$Diff, col = "navy", main = "Boxplot of DraftKings Points")